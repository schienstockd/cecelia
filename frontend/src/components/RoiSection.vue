<!--
  The viewer's ROI list (docs/todo/ROI_TRIAGE_PLAN.md §7) — the body of the viewer window's "ROIs"
  side-rail section. Lists the ROIs a "Find ROIs" run wrote for this image, steps through them with
  prev/next (reviewPager), and moves the viewer to each one: seek t/z + fit the box
  (buildFocusViewState) + outline its objects (pick highlight). Accept / reject / flag / label / note
  go through POST /api/rois/review — the same surface Claude writes through, attributed server-side.
-->
<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import SelectionTable, { type SelectionColumn } from './SelectionTable.vue'
import ChipSelect, { type ChipOption } from './ChipSelect.vue'
import { useViewerStore } from '../stores/viewer'
import { useSettingsStore } from '../stores/settings'
import { useWsStore } from '../stores/ws'
import { useToast } from '../composables/useToast'
import { useFieldDraft } from '../composables/useFieldDraft'
import { buildFocusViewState } from '../utils/viewer/focusOnCell'
import { nextIndex, prevIndex, clampIndex, pageSummary } from '../utils/reviewPager'
import {
  fetchRois, postRoiReview, filterRois, roiFocusTarget, claudeLabelText, disagrees,
  ROI_STATUSES, ROI_TYPES, type RoiChange, type RoiList, type RoiRow, type RoiStatus, type RoiType,
} from '../utils/rois'

const props = defineProps<{ projectUid: string, imageUid: string }>()

const viewer = useViewerStore()
const settings = useSettingsStore()
const ws = useWsStore()
const toast = useToast()

const list = ref<RoiList | null>(null)
const loading = ref(false)
const activeId = ref('')

async function load() {
  if (!props.projectUid || !props.imageUid) return
  loading.value = true
  list.value = await fetchRois(props.projectUid, props.imageUid, settings.viewerRoiSuffix)
  loading.value = false
}

const rows = computed(() =>
  filterRois(list.value?.rois ?? [], settings.viewerRoiStatus as RoiStatus | 'all'))
const index = computed(() => clampIndex(rows.value.findIndex(r => r.id === activeId.value), rows.value.length))
const active = computed<RoiRow | null>(() => {
  const i = rows.value.findIndex(r => r.id === activeId.value)
  return i >= 0 ? rows.value[i] : null
})

function goto(r: RoiRow) {
  activeId.value = r.id
  const next = buildFocusViewState(viewer.viewState, roiFocusTarget(r))
  if (next) viewer.setPendingViewState({ viewState: next, imageUid: props.imageUid })
  const vn = list.value?.valueName
  if (vn) {
    viewer.setPickHighlight({ imageUid: props.imageUid, valueName: vn, labels: r.objectLabels,
                              focusId: 0, label: `ROI ${r.rank}`, origin: 'user' })
  }
}
function step(dir: 1 | -1) {
  const n = rows.value.length
  if (!n) return
  const cur = active.value ? index.value : (dir === 1 ? -1 : 0)
  const i = dir === 1 ? nextIndex(cur, n) : prevIndex(cur, n)
  if (i >= 0) goto(rows.value[i])
}

async function review(r: RoiRow, change: RoiChange) {
  const err = await postRoiReview(props.projectUid, props.imageUid, list.value?.suffix ?? '', r.id, change)
  if (err) toast.add({ severity: 'error', summary: 'ROI not updated', detail: err, life: 5000 })
  await load()
}
const setStatus = (r: RoiRow, s: 'accepted' | 'rejected' | 'flagged' | 'pending') =>
  review(r, { status: r.status === s ? 'pending' : s })

const columns: SelectionColumn[] = [
  { key: 'rank', label: '#', sortable: true, width: 34 },
  { key: 'site', label: 'Site', sortable: true, width: 40 },
  { key: 'cue', label: 'Cue', sortable: true, width: 60 },
  { key: 'frames', label: 't', sortKey: 't0', sortable: true, width: 56 },
  { key: 'status', label: 'Status', sortable: true, width: 70 },
  { key: 'claude', label: 'Claude', ellipsis: true },
]
const tableRows = computed(() => rows.value.map(r => ({
  id: r.id, rank: r.rank, site: r.site ?? r.rank, t0: r.t0, status: r.status,
  cue: r.type, frames: `${r.t0}–${r.t1}`,
  claude: claudeLabelText(r.review) + (disagrees(r.review) ? ' ≠' : ''),
})))
const rowTooltip = (row: Record<string, unknown>) => {
  const r = rows.value.find(x => x.id === row.id)
  return r?.review?.claudeLabel?.reason ?? `${r?.nObjects ?? 0} objects · score ${r?.score.toFixed(1) ?? ''}`
}

const STATUS_LABEL: Record<RoiStatus, string> = {
  'pending': 'pending', 'claude-labelled': 'Claude', 'accepted': 'accepted', 'rejected': 'rejected', 'flagged': 'flagged',
}
const statusOptions = computed<ChipOption[]>(() => [
  { value: 'all', label: 'all', tip: 'Every ROI', badge: list.value?.rois.length ?? 0 },
  ...ROI_STATUSES.map(s => ({ value: s, label: STATUS_LABEL[s], tip: `ROIs that are ${s}`,
                               badge: (list.value?.rois ?? []).filter(r => r.status === s).length })),
])
// the committed note is the source; a list reload while typing leaves the draft alone (useFieldDraft)
const noteDraft = useFieldDraft(() => active.value?.review?.note ?? '')
function commitNote() {
  const r = active.value
  if (r && noteDraft.value !== (r.review?.note ?? '')) review(r, { note: noteDraft.value })
}

function onChanged(data: Record<string, unknown>) {
  if (String(data.imageUid ?? '') === props.imageUid) load()
}
onMounted(() => { load(); ws.on('rois:changed', onChanged) })
onBeforeUnmount(() => ws.off('rois:changed', onChanged))
watch(() => settings.viewerRoiSuffix, load)
</script>

<template>
  <div class="roi-sec">
    <template v-if="list && list.rois.length">
      <div class="cc-row cc-row-tight">
        <select v-if="list.suffixes.length > 1" v-model="settings.viewerRoiSuffix" class="cc-input-2xs"
                v-tooltip.bottom="'Which ROI set'">
          <option v-for="s in list.suffixes" :key="s" :value="s">{{ s }}</option>
        </select>
        <span class="roi-grow" />
        <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.bottom="'Previous ROI'"
                aria-label="Previous ROI" @click="step(-1)"><i class="pi pi-chevron-left" /></button>
        <span class="cc-readout cc-fs-2xs">{{ pageSummary(index, rows.length) }}</span>
        <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.bottom="'Next ROI'"
                aria-label="Next ROI" @click="step(1)"><i class="pi pi-chevron-right" /></button>
      </div>

      <ChipSelect v-model="settings.viewerRoiStatus" :options="statusOptions" variant="pill" />
      <SelectionTable :columns="columns" :rows="tableRows" id-key="id" :model-value="activeId"
                      density="compact" column-width-key="cc.vw.rois.colw" :row-tooltip="rowTooltip"
                      @update:model-value="(id: string) => { const r = rows.find(x => x.id === id); if (r) goto(r) }" />

      <div v-if="active" class="roi-detail">
        <div class="cc-row cc-row-tight">
          <span class="cc-eyebrow cc-fs-2xs">ROI {{ active.rank }}</span>
          <span class="cc-muted cc-fs-3xs">{{ active.nObjects }} objects · z {{ active.box.z0 }}–{{ active.box.z1 }}</span>
          <span class="roi-grow" />
          <button class="cc-btn cc-btn-bare cc-btn-icon" :class="{ 'cc-btn-on': active.status === 'accepted' }"
                  v-tooltip.bottom="'Accept'" aria-label="Accept" @click="setStatus(active, 'accepted')"><i class="pi pi-check" /></button>
          <button class="cc-btn cc-btn-bare cc-btn-icon" :class="{ 'cc-btn-on': active.status === 'rejected' }"
                  v-tooltip.bottom="'Reject'" aria-label="Reject" @click="setStatus(active, 'rejected')"><i class="pi pi-times" /></button>
          <button class="cc-btn cc-btn-bare cc-btn-icon" :class="{ 'cc-btn-on': active.status === 'flagged' }"
                  v-tooltip.bottom="'Flag for a second look'" aria-label="Flag" @click="setStatus(active, 'flagged')"><i class="pi pi-flag" /></button>
          <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.bottom="'Undo the last change'"
                  aria-label="Undo" @click="review(active, { undo: true })"><i class="pi pi-undo" /></button>
        </div>
        <div v-if="active.review?.claudeLabel" class="cc-muted cc-fs-3xs">
          Claude: {{ claudeLabelText(active.review) }} — {{ active.review.claudeLabel.reason }}
        </div>
        <div class="cc-row cc-row-tight">
          <span class="cc-muted cc-fs-2xs cc-lbl-col">Label</span>
          <select :value="active.review?.userLabel ?? ''" class="cc-input-2xs roi-grow" v-tooltip.bottom="'Your label for this ROI'"
                  @change="e => review(active!, { userLabel: (e.target as HTMLSelectElement).value as RoiType | '' })">
            <option value="">—</option>
            <option v-for="t in ROI_TYPES" :key="t" :value="t">{{ t.replace('_', ' ') }}</option>
          </select>
        </div>
        <div class="cc-row cc-row-tight">
          <span class="cc-muted cc-fs-2xs cc-lbl-col">Note</span>
          <input v-model="noteDraft" class="cc-input-2xs roi-grow" v-tooltip.bottom="'Saved when you leave the field'"
                 @change="commitNote">
        </div>
      </div>
    </template>
    <div v-else-if="!loading" class="cc-empty-inline cc-fs-2xs">
      No ROIs yet — run Find ROIs
    </div>
  </div>
</template>

<style scoped>
.roi-sec { display: flex; flex-direction: column; gap: 0.3rem; min-width: 0; }
.roi-grow { flex: 1; min-width: 0; }
.roi-detail { display: flex; flex-direction: column; gap: 0.25rem; }
</style>
