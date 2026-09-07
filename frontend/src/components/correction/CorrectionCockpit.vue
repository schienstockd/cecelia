<!--
  Correction cockpit — one panel to drive all track and (soon) label corrections.

  Phase 1a landed the shell (FloatingPanel, mode radio, empty tool grid, queue footer). Phase 1b
  (this) wires the Tracks-mode verbs: value-name picker at the top, then Show / Add / Join / Split
  / Remove buttons that operate on whatever the timeline currently has selected. Phase 1c will
  remove the timeline's action row so the cockpit is the sole authoring surface; Phase 2 adds
  Labels ops; Phase 3 adds Review (sort + pager + Details); Phase 4 adds brush tools.

  How the two surfaces cooperate right now. The `useCorrectionCockpitStore` holds the shared
  `(image, valueName)`-keyed state: selected tracks, split-frame, det-selection, plus the computed
  Join/Split/Remove/Add ops with their `blocked` reasons. TrackSchemeView PUBLISHES to this store
  on every selection / splitAt / detSel change (writes only, Phase 1b). The cockpit READS the same
  store and renders the ops as buttons — one implementation of the validation rules, no refetches
  of the paths, no chance of the two surfaces disagreeing about what's Joinable.

  Scope: `openImageUid` (viewer's currently-focused image) + a valueName picked here or defaulted
  by `useTrackValueNames`. When either is missing the cockpit reads as "open an image", and every
  button disables — nothing does a partial write.
-->
<script setup lang="ts">
import { computed } from 'vue'
import FloatingPanel from '../FloatingPanel.vue'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
import { useProjectStore } from '../../stores/project'
import { useSettingsStore } from '../../stores/settings'
import { useTrackOpsQueueStore, trackOpsKey } from '../../stores/trackOpsQueue'
import { useCorrectionCockpitStore } from '../../stores/correctionCockpit'
import { useTrackValueNames } from '../../composables/useTrackValueNames'
import { undoLast, opDescription, type TrackOp } from '../../lib/trackCorrection'
import { submitTrackOps } from '../../lib/trackOpsRun'
import { showTracksInViewer } from '../../utils/viewer/showTracksInViewer'

const emit = defineEmits<{ close: [] }>()

const project = useProjectStore()
const settings = useSettingsStore()
const queueStore = useTrackOpsQueueStore()
const cockpit = useCorrectionCockpitStore()

const projectUid = computed(() => project.loadedProjectUid ?? '')
const imageUid = computed(() => project.openImageUid ?? '')
const setUid = computed(() => imageUid.value ? (project.setUidOfImage(imageUid.value) ?? null) : null)

// Picker state — starts empty and `useTrackValueNames.resolved()` picks the fallback (active
// segmentation if tracked, else first tracked, else first-of-anything). Persisted in settings for
// symmetry with the mode setting; a bare ref would forget the user's choice on remount.
const wantedValueName = computed<string>({
  get: () => settings.correctionCockpitValueName,
  set: v => { settings.correctionCockpitValueName = v },
})
const { valueNames, trackedNames, resolved } = useTrackValueNames(projectUid, imageUid, wantedValueName)
const valueName = computed(() => resolved())

const key = computed(() => trackOpsKey(projectUid.value, imageUid.value, valueName.value))
const scope = computed(() => cockpit.state(key.value))
const pending = computed(() => queueStore.get(key.value))
const pendingCount = computed(() => pending.value.length)

const mode = computed<'tracks' | 'labels' | 'review'>({
  get: () => settings.correctionCockpitMode,
  set: v => { settings.correctionCockpitMode = v },
})

const MODES: ChipOption[] = [
  { value: 'tracks', label: 'Tracks', tip: 'Join, split, remove, points' },
  { value: 'labels', label: 'Labels', tip: 'Merge, remove, draw — Phase 2+', disabled: true },
  { value: 'review', label: 'Review', tip: 'Sort + prev/next + Details — Phase 3', disabled: true },
]

const valueNameOptions = computed<ChipOption[]>(() => {
  // tracked names first (the ones you can actually author against), then the rest so the picker
  // still names them (an image with an untracked segmentation reads as "not tracked" against a
  // real segmentation, not against nothing — same principle as `resolveTrackValueName`).
  const tracked = new Set(trackedNames.value)
  const trackedList = valueNames.value.filter(n => tracked.has(n))
  const otherList = valueNames.value.filter(n => !tracked.has(n))
  return [
    ...trackedList.map(n => ({ value: n, label: n })),
    ...otherList.map(n => ({ value: n, label: n, tip: 'Not tracked' })),
  ]
})

const selectedSummary = computed(() => {
  const ids = scope.value.selectedTracks
  if (!ids.length) return 'No track selected'
  if (ids.length === 1) return `Track ${ids[0]}${scope.value.splitFrame !== null ? ` @ frame ${scope.value.splitFrame}` : ''}`
  if (ids.length === 2) return `Tracks ${ids.join(' + ')}`
  return `${ids.length} tracks selected`
})

const detSummary = computed(() => {
  const d = scope.value.detSelection
  if (!d) return ''
  return `Untracked frame ${d.frame} · ${d.labels.length} label${d.labels.length === 1 ? '' : 's'}`
})

function queueOp(op: TrackOp | null): void {
  if (!op || !key.value) return
  queueStore.set(key.value, [...queueStore.get(key.value), op])
}

async function showInViewer(): Promise<void> {
  const ids = scope.value.selectedTracks.map(Number).filter(Number.isFinite)
  if (!ids.length) return
  await showTracksInViewer(projectUid.value, imageUid.value, valueName.value, ids, 'cockpit')
}

function onUndo(): void {
  if (!pending.value.length) return
  queueStore.set(key.value, undoLast(pending.value))
}
function onClear(): void {
  queueStore.clear(key.value)
}
function onApply(): void {
  if (!pending.value.length) return
  const ok = submitTrackOps({
    projectUid: projectUid.value, setUid: setUid.value, imageUid: imageUid.value,
    valueName: valueName.value, ops: pending.value, source: 'cockpit',
  })
  if (ok) queueStore.clear(key.value)
}

function shortId(uid: string): string { return uid.length > 8 ? uid.slice(0, 6) + '…' : uid }

// Tool metadata — the Tracks-mode buttons. Icons-only where a glyph reads unambiguously (Show,
// Add, Remove — the registered ones from iconLegend.ts); label-only for Join/Split, per the
// CORRECTION_PLAN.md §440 decision that no PrimeIcons glyph maps to "merge these two" or "cut
// this in half" without colliding with an existing meaning. Same as the timeline's row today.
type ToolRow = {
  key: string; label: string; icon?: string; showLabel?: boolean
  blocked: string; op: TrackOp | null; run: () => void
}

const showTool = computed<ToolRow>(() => ({
  key: 'show', label: 'Show', icon: 'pi-eye', showLabel: true,
  blocked: scope.value.selectedTracks.length ? '' : 'Pick at least one track first',
  op: null,
  run: showInViewer,
}))
const addTool = computed<ToolRow>(() => {
  const add = scope.value.addAction
  return {
    key: 'add', label: add.label, icon: 'pi-plus', showLabel: true,   // label varies (as new track / first of N / …)
    blocked: add.blocked || '',
    op: add.op,
    run: () => queueOp(add.op),
  }
})
const joinSplitRemoveTools = computed<ToolRow[]>(() =>
  scope.value.actions.map(a => ({
    key: a.key, label: a.label,
    icon: a.key === 'remove' ? 'pi-trash' : undefined,   // Join/Split: no glyph (CORRECTION_PLAN.md §440)
    showLabel: true,   // every button carries its label — one visual rule across the toolbar
    blocked: a.blocked || '',
    op: a.op,
    run: () => queueOp(a.op),
  }))
)

const tools = computed<ToolRow[]>(() => [
  showTool.value,
  addTool.value,
  ...joinSplitRemoveTools.value,
])
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
          <div v-if="!key" class="cockpit-placeholder cc-fs-sm cc-muted">
            Pick a tracked segmentation to enable the verbs.
          </div>
          <div v-else class="cc-btn-group cockpit-toolbar">
            <button v-for="t in tools" :key="t.key"
                    class="cc-btn cc-btn-dense"
                    :class="[t.blocked ? 'cc-btn-bare' : (t.key === 'remove' ? 'cc-btn-danger-ghost' : 'cc-btn-primary'),
                             t.icon && !t.showLabel ? 'cc-btn-icon' : '']"
                    :disabled="!!t.blocked"
                    v-tooltip.top="t.blocked || (t.op ? opDescription(t.op) : t.label)"
                    @click="t.run()">
              <i v-if="t.icon" :class="['pi', t.icon]" />
              <span v-if="t.showLabel">{{ t.label }}</span>
            </button>
          </div>
        </template>
        <div v-else-if="mode === 'labels'" class="cockpit-placeholder cc-fs-sm cc-muted">
          Phase 2: Select · Merge · Remove.<br />
          Phase 4: Draw · Erase · Fill · Pick.
        </div>
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
