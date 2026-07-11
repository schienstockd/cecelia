<script setup lang="ts">
import { ref, computed, watch, onMounted, onUnmounted } from 'vue'
import { useProjectStore, type CciaImage } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useLogStore } from '../stores/log'
import { useTaskStore, type TaskStatus } from '../stores/tasks'
import { metadataWarning } from '../lib/imageMetadataWarnings'
import { qcSummary } from '../lib/qc'
import { isExcluded, isIncluded, includedUids } from '../utils/inclusion'
import { timelapseDuration } from '../utils/imageTable'
import { useNapariOpen } from '../composables/useNapariOpen'
import PhysicalSizeDialog from './PhysicalSizeDialog.vue'

const props = defineProps<{
  setUid: string
  module?: string      // when provided, shows a per-module status column
  showAttrs?: boolean  // show per-channel + attr columns
  editableMeta?: boolean // allow inline editing of attr + channel-name cells (Metadata page only)
  allowDelete?: boolean
  filterUids?: string[] // when provided, restricts visible rows to these UIDs
  singleSelect?: boolean // radio-style: at most one image selected (e.g. gating)
  selectionScope?: string // namespace for remembering the selection (e.g. module name)
}>()
const emit = defineEmits<{ (e: 'selectionChange', uids: string[]): void }>()

const project     = useProjectStore()
const projectMeta = useProjectMetaStore()
const log         = useLogStore()
const taskStore   = useTaskStore()

// opens right where you are — no page navigation needed
const physSizeDialogUid = ref<string | null>(null)

// Two distinct affordances, kept visually separate: the warning (any module, always visible when
// flagged) sits in front of the name where it's impossible to miss; the neutral "open editor" icon
// lives after the name alongside the other row-hover actions (copy UID) on every row — flagged or
// not — on the pages where reviewing/propagating physical size is a primary task, so a known-good
// image can be opened deliberately and used as the Copy/Fill-flagged reference.
function warnIconFor(img: CciaImage): { tip: string } | null {
  const w = metadataWarning(img)
  return w ? { tip: w.short } : null
}
// QC badge — advisory "we processed this but the output looks off" (docs/todo/QC_PLAN.md). Distinct
// from the metadata warning: any module can emit it, and it's non-blocking (hover for detail).
function qcFor(img: CciaImage): { short: string; long: string; level: 'info' | 'warn' } | null {
  return qcSummary(img)
}
function pageIconFor(): { tip: string } | null {
  if (props.module === 'metadata' || props.module === 'import')
    return { tip: 'View or edit physical size & timing' }
  return null
}

// ── Selection ─────────────────────────────────────────────────────────────────

const selected  = ref<Set<string>>(new Set())
const deleteUid = ref<string | null>(null)
const napariLoading = ref<Set<string>>(new Set())
const copiedUid = ref<string | null>(null)

async function copyUid(uid: string) {
  try {
    await navigator.clipboard.writeText(uid)
  } catch {
    const ta = document.createElement('textarea')
    ta.value = uid
    ta.style.position = 'fixed'
    ta.style.opacity = '0'
    document.body.appendChild(ta)
    ta.select()
    document.execCommand('copy')
    document.body.removeChild(ta)
  }
  copiedUid.value = uid
  setTimeout(() => { copiedUid.value = null }, 1200)
}

// ── Inline cell editing ─────────────────────────────────────────────────────────
// One generic core (click a cell → edit → Enter/blur commits, Esc cancels) reused by attributes,
// channel names, AND the exclusion note. Each field only supplies how to persist its value (`save*`
// below), so there's no per-field copy of the edit lifecycle. `key` is namespaced per field type.
const editingCell = ref<string | null>(null)        // `${uid}:${key}` currently being edited
const editValue = ref('')
const cellKey = (uid: string, key: string) => `${uid}:${key}`
const isEditing = (uid: string, key: string) => editingCell.value === cellKey(uid, key)
function startEdit(uid: string, key: string, current: string) {
  editingCell.value = cellKey(uid, key)
  editValue.value = current
}
function cancelEdit() { editingCell.value = null }
// Focus the edit input when it mounts, without stealing focus if it already has it.
// (Lives here, not in the template, because the template scope doesn't expose `document`.)
function focusEditInput(el: unknown) {
  const i = el as HTMLInputElement | null
  if (i && i !== document.activeElement) i.focus()
}
// Generic commit: guard it's still this cell, no-op when unchanged, else delegate to `save(val)`.
async function commitEdit(uid: string, key: string, current: string, save: (val: string) => Promise<void>) {
  if (!isEditing(uid, key)) return
  editingCell.value = null
  const val = editValue.value.trim()
  if ((current ?? '') === val) return
  await save(val)
}

// Per-field savers — the only thing that differs between editable cells.
async function saveAttr(img: CciaImage, key: string, val: string) {
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  try {
    const res = await fetch('/api/images/attr/set', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, attrName: key, values: { [img.uid]: val } }),
    })
    if (!res.ok) throw new Error((await res.json()).error ?? res.statusText)
    project.setAttrValues(key, { [img.uid]: val })               // reflect immediately
  } catch (e) {
    log.error(`Failed to set ${key}: ${e instanceof Error ? e.message : String(e)}`, { source: 'import' })
  }
}
// Channel names are one list per image; editing a single column replaces that index (1-based) and
// re-sends the whole list (the /channelnames endpoint is list-valued). Pads if naming a later channel.
function channelEditable(img: CciaImage, idx: number): boolean {
  return idx <= Math.max(img.channelNames?.length ?? 0, img.sizeC ?? 0)
}
async function saveChannel(img: CciaImage, idx: number, val: string) {
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  const names = [...(img.channelNames ?? [])]
  while (names.length < idx) names.push('')
  names[idx - 1] = val
  try {
    const res = await fetch('/api/images/channelnames', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, imageUids: [img.uid], channelNames: names }),
    })
    if (!res.ok) throw new Error((await res.json()).error ?? res.statusText)
    project.updateImageMeta(img.uid, { channelNames: names })     // reflect immediately
  } catch (e) {
    log.error(`Failed to set channel ${idx}: ${e instanceof Error ? e.message : String(e)}`, { source: 'import' })
  }
}
async function saveNote(img: CciaImage, val: string) {
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  const prev = img.note ?? ''
  project.setInclusion(img.uid, { note: val })                   // reflect immediately
  try {
    const res = await fetch('/api/images/inclusion/set', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, values: { [img.uid]: { note: val } } }),
    })
    if (!res.ok) throw new Error((await res.json()).error ?? res.statusText)
  } catch (e) {
    project.setInclusion(img.uid, { note: prev })                // revert on failure
    log.error(`Failed to save note: ${e instanceof Error ? e.message : String(e)}`, { source: 'import' })
  }
}

// ── Include / exclude ─────────────────────────────────────────────────────────
// Excluded images stay visible but greyed, can't be selected, and are skipped by every run.
const NOTE_KEY = '__note'

// run-history popover (cog after the uid) — automatic per-image provenance (project store `runLog`).
// position: fixed from the cog so it escapes the table's horizontal scroll clipping.
const runLogUid = ref<string | null>(null)
const runLogPos = ref<Record<string, string>>({})
// the image whose run-history popover is open (the popover is teleported to <body>, so it reads this)
const runLogImg = computed(() => runLogUid.value ? (images.value.find(i => i.uid === runLogUid.value) ?? null) : null)
const fmtRunAt = (at: string) => (at ?? '').replace('T', ' ')
function toggleRunLog(uid: string, e: MouseEvent) {
  if (runLogUid.value === uid) { runLogUid.value = null; return }
  const r = (e.currentTarget as HTMLElement).getBoundingClientRect()
  runLogPos.value = { position: 'fixed', top: `${Math.round(r.bottom + 4)}px`, left: `${Math.round(r.left)}px` }
  runLogUid.value = uid
}
function onDocMouseDownRunLog(e: MouseEvent) {
  const t = e.target as HTMLElement
  if (runLogUid.value && !t.closest('.runlog-cell') && !t.closest('.runlog-pop')) runLogUid.value = null
}
onMounted(() => document.addEventListener('mousedown', onDocMouseDownRunLog))
onUnmounted(() => document.removeEventListener('mousedown', onDocMouseDownRunLog))
async function setIncluded(img: CciaImage, included: boolean) {
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  project.setInclusion(img.uid, { included })                 // reflect immediately
  if (!included && selected.value.has(img.uid)) {             // drop from selection on exclude
    selected.value.delete(img.uid)
    selected.value = new Set(selected.value)
    commit()
  }
  try {
    const res = await fetch('/api/images/inclusion/set', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, values: { [img.uid]: { included } } }),
    })
    if (!res.ok) throw new Error((await res.json()).error ?? res.statusText)
  } catch (e) {
    project.setInclusion(img.uid, { included: !included })     // revert on failure
    log.error(`Failed to ${included ? 'include' : 'exclude'} image: ${e instanceof Error ? e.message : String(e)}`, { source: 'import' })
  }
}

const images = computed(() => {
  const all = project.sets.find(s => s.uid === props.setUid)?.images ?? []
  if (!props.filterUids) return all
  const keep = new Set(props.filterUids)
  return all.filter(i => keep.has(i.uid))
})

// ── Remembered selection ────────────────────────────────────────────────────
// Persist the checkbox selection in the project store (keyed by scope + set) so it survives
// navigating away from the page and back. `commit` is called after every change; `seed` restores
// it on mount and when the set switches (dropping any UIDs no longer present, and capping to one
// in single-select mode).
const scope = computed(() => props.selectionScope ?? 'default')
function commit() {
  project.setImageSelection(scope.value, props.setUid, [...selected.value])
  emit('selectionChange', [...selected.value])
}
function seed() {
  const stored = project.getImageSelection(scope.value, props.setUid)
  const imgs = images.value
  // keep only stored uids that are still present AND still included (an image excluded while away
  // shouldn't come back selected)
  let uids = imgs.length
    ? stored.filter(u => imgs.some(i => i.uid === u && (canSelectExcluded.value || isIncluded(i))))
    : stored
  if (props.singleSelect && uids.length > 1) uids = [uids[0]]
  selected.value = new Set(uids)
  emit('selectionChange', [...selected.value])
}
onMounted(seed)
watch(() => props.setUid, seed)
// Re-seed when the stored selection is changed from OUTSIDE the table (e.g. the cluster page's
// "select clustered images" button writes the store directly). Guard against our own commit()
// writes: only re-seed when the store differs from the current checkbox set, so this never loops.
watch(() => project.getImageSelection(scope.value, props.setUid).join(','), (csv) => {
  const stored = csv ? csv.split(',') : []
  if (stored.length === selected.value.size && stored.every(u => selected.value.has(u))) return
  seed()
})

// On the import + metadata pages excluded images ARE selectable (you curate/edit metadata there,
// incl. on excluded ones); everywhere else selection is the runnable (included) subset only.
const canSelectExcluded = computed(() => props.module === 'import' || props.module === 'metadata')

// Selectable set for select-all + the "all/some" header state: all images where excluded are
// selectable (import/metadata), else only the included (runnable) subset.
const selectableUids = computed(() =>
  canSelectExcluded.value ? images.value.map(i => i.uid) : includedUids(images.value))
const allSelected = computed(() =>
  selectableUids.value.length > 0 && selectableUids.value.every(u => selected.value.has(u))
)
const someSelected = computed(() =>
  selected.value.size > 0 && !allSelected.value
)

function toggleAll() {
  selected.value = allSelected.value ? new Set() : new Set(selectableUids.value)
  commit()
}

// Quick way to batch-fix physical-size/timing warnings: select every flagged image in one click,
// then open a clean reference image's dialog and Copy/Fill flagged onto exactly this selection.
const flaggedUids = computed(() => images.value.filter(i => metadataWarning(i) && isIncluded(i)).map(i => i.uid))
// "Active" = the current selection IS exactly the flagged set — drives both the toggle behaviour
// and the icon colour (gray = not applied, amber = applied).
const flaggedActive = computed(() =>
  flaggedUids.value.length > 0 &&
  selected.value.size === flaggedUids.value.length &&
  flaggedUids.value.every(u => selected.value.has(u))
)
function selectFlagged() {
  selected.value = flaggedActive.value ? new Set() : new Set(flaggedUids.value)
  commit()
}

// For images imported before physical-size/timing metadata was tracked in ccid.json (or whose
// meta lost these fields): the OME-ZARR itself is already correct, so re-derive `meta` straight
// from it (same reader the importer uses) instead of asking the user to re-import or type values
// back in by hand.
const resyncing = ref(false)
async function resyncFlagged() {
  const uids = flaggedUids.value
  const projectUid = projectMeta.current?.uid
  if (!uids.length || !projectUid || resyncing.value) return
  resyncing.value = true
  try {
    const res = await fetch('/api/images/meta/resync', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, imageUids: uids }),
    })
    const body = await res.json().catch(() => ({})) as { ok?: boolean; images?: Record<string, Partial<CciaImage>>; error?: string }
    if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
    for (const [uid, img] of Object.entries(body.images ?? {})) {
      project.updateImageMeta(uid, {
        physicalSizeX: img.physicalSizeX,
        physicalSizeY: img.physicalSizeY,
        physicalSizeZ: img.physicalSizeZ,
        physicalSizeUnit: img.physicalSizeUnit,
        physicalSizeZCorrected: img.physicalSizeZCorrected,
        timeIncrement: img.timeIncrement,
        timeIncrementUnit: img.timeIncrementUnit,
      })
    }
    log.info(`Re-read physical size & timing from file for ${uids.length} image(s).`, { source: 'import' })
  } catch (e) {
    log.error(`Failed to resync metadata: ${e instanceof Error ? e.message : String(e)}`, { source: 'import' })
  } finally {
    resyncing.value = false
  }
}

function toggle(uid: string) {
  const img = images.value.find(i => i.uid === uid)
  if (img && isExcluded(img) && !canSelectExcluded.value) return   // excluded → not runnable (except import/metadata)
  if (props.singleSelect) {
    // radio-style: clicking selects only this image (clicking the selected one clears it)
    selected.value = selected.value.has(uid) ? new Set() : new Set([uid])
  } else if (selected.value.has(uid)) {
    selected.value.delete(uid)
    selected.value = new Set(selected.value)
  } else {
    selected.value.add(uid)
    selected.value = new Set(selected.value)
  }
  commit()
}

// ── Delete ────────────────────────────────────────────────────────────────────

function confirmDelete(uid: string) { deleteUid.value = uid }

async function doDelete() {
  if (!deleteUid.value) return
  const uid = deleteUid.value
  const img = images.value.find(i => i.uid === uid)
  deleteUid.value = null
  const projectUid = projectMeta.current?.uid
  if (projectUid) {
    try {
      const res = await fetch('/api/images/delete', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ projectUid, setUid: props.setUid, imageUid: uid }),
      })
      const body = await res.json().catch(() => ({})) as { error?: string }
      if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
    } catch (e) {
      log.error(`Failed to delete image: ${e instanceof Error ? e.message : String(e)}`, { source: 'import' })
      return
    }
  }
  project.deleteImage(props.setUid, uid)
  selected.value.delete(uid)
  commit()
  log.info(`Removed "${img?.name}".`, { source: 'import' })
}

// ── Napari ────────────────────────────────────────────────────────────────────

const { openInNapari: napariOpen } = useNapariOpen()   // shared open path (see composable)
async function openInNapari(imageUid: string) {
  // reload short-circuits inside the composable too; skip the loading spinner for a reload
  if (project.napariImageUid === imageUid) { project.requestNapariReload(); return }
  napariLoading.value = new Set([...napariLoading.value, imageUid])
  try {
    await napariOpen(imageUid, props.setUid)
  } finally {
    napariLoading.value = new Set([...napariLoading.value].filter(u => u !== imageUid))
  }
}

// ── Status ────────────────────────────────────────────────────────────────────

function imageModuleStatus(img: CciaImage): TaskStatus | 'pending' | null {
  if (!props.module) return null
  const t = taskStore.forModule(props.module, projectMeta.current?.uid).find(t => t.imageUid === img.uid)
  if (t) return t.status
  if (props.module === 'import') {
    const s = img.status as string
    if (s === 'converting') return 'running'
    if (s === 'done')       return 'done'
    if (s === 'failed')     return 'failed'
    return 'pending'
  }
  return null
}

const statusConfig: Record<string, { label: string; cls: string }> = {
  pending:   { label: 'Pending',   cls: 'st-pending'  },
  queued:    { label: 'Queued',    cls: 'st-queued'   },
  running:   { label: 'Active',    cls: 'st-running'  },
  done:      { label: 'Done',      cls: 'st-done'     },
  failed:    { label: 'Failed',    cls: 'st-failed'   },
  cancelled: { label: 'Cancelled', cls: 'st-pending'  },
}

// ── Dynamic columns (attrs + channels) ───────────────────────────────────────

// Max channel count across the set — determines how many channel columns to show.
const channelCount = computed(() => {
  if (!props.showAttrs) return 0
  return images.value.reduce((max, img) =>
    Math.max(max, img.channelNames?.length ?? 0, img.sizeC ?? 0), 0)
})

// 1-based channel indices, e.g. [1, 2, 3]
const channelIndices = computed(() =>
  Array.from({ length: channelCount.value }, (_, i) => i + 1)
)

// Dimension columns only appear when the set actually has a z-stack / timelapse (mirrors the
// metadata-warning z>1 / t>1 tests), so 2D single-timepoint sets aren't cluttered with empty columns.
const anyZStack    = computed(() => images.value.some(i => (i.sizeZ ?? 0) > 1))
const anyTimelapse = computed(() => images.value.some(i => (i.sizeT ?? 0) > 1))

// Union of attr keys across the set, sorted.
const attrKeys = computed(() => {
  if (!props.showAttrs) return []
  const keys = new Set<string>()
  for (const img of images.value)
    for (const k of Object.keys(img.attr ?? {})) keys.add(k)
  return [...keys]
})

// ── Column resize ─────────────────────────────────────────────────────────────

// Keyed by column id (e.g. 'name', 'ch-1', 'attr-condition')
const colWidths = ref<Record<string, number>>({})

const DEFAULT_WIDTHS: Record<string, number> = {
  name: 200,
}
function defaultWidth(key: string): number {
  if (DEFAULT_WIDTHS[key]) return DEFAULT_WIDTHS[key]
  if (key.startsWith('ch-'))   return 90
  if (key.startsWith('attr-')) return 110
  return 100
}
function colW(key: string): string {
  return (colWidths.value[key] ?? defaultWidth(key)) + 'px'
}

let _resize: { key: string; startX: number; startW: number } | null = null

function startResize(key: string, e: MouseEvent) {
  _resize = { key, startX: e.clientX, startW: colWidths.value[key] ?? defaultWidth(key) }
  document.addEventListener('mousemove', doResize)
  document.addEventListener('mouseup', stopResize)
  document.body.style.userSelect = 'none'
  document.body.style.cursor = 'col-resize'
  e.preventDefault()
}

function doResize(e: MouseEvent) {
  if (!_resize) return
  const w = Math.max(40, _resize.startW + (e.clientX - _resize.startX))
  colWidths.value = { ...colWidths.value, [_resize.key]: w }
}

function stopResize() {
  _resize = null
  document.removeEventListener('mousemove', doResize)
  document.removeEventListener('mouseup', stopResize)
  document.body.style.userSelect = ''
  document.body.style.cursor = ''
}

onUnmounted(stopResize)
</script>

<template>
  <!-- delete confirmation -->
  <div v-if="deleteUid" class="delete-confirm">
    <span>Remove <strong>{{ images.find(i => i.uid === deleteUid)?.name }}</strong>?</span>
    <button class="btn-danger btn-sm" @click="doDelete"
      v-tooltip.right="'Remove this image from the set. The original file is not deleted.'">
      Remove
    </button>
    <button class="btn-ghost btn-sm" @click="deleteUid = null">Cancel</button>
  </div>

  <div v-if="images.length === 0" class="empty-state">
    <i class="pi pi-images empty-icon" />
    <p class="empty-title">No images in this set</p>
    <p class="empty-hint">Use <strong>Add images</strong> in the import module to add files.</p>
  </div>

  <div v-else class="table-scroll">
  <table class="image-table">
    <thead>
      <tr>
        <!-- fixed: checkbox (no select-all in single-select mode) -->
        <th class="col-fixed col-check">
          <input v-if="!singleSelect" type="checkbox"
            :checked="allSelected" :indeterminate="someSelected"
            @change="toggleAll"
            v-tooltip.right="'Select / deselect all.'"
          />
        </th>

        <!-- fixed: napari eye -->
        <th class="col-fixed col-viewer" />

        <!-- resizable: name (frozen-left) -->
        <th class="col-resize col-name" :style="{ width: colW('name'), minWidth: colW('name') }">
          Name
          <button v-if="!singleSelect && flaggedUids.length" class="select-flagged-btn"
            :class="{ active: flaggedActive }" @click.stop="selectFlagged"
            v-tooltip.bottom="flaggedActive ? 'Deselect flagged images' : `Select all ${flaggedUids.length} flagged image(s)`">
            <i class="pi pi-exclamation-triangle" />
          </button>
          <button v-if="flaggedUids.length && (module === 'metadata' || module === 'import')"
            class="select-flagged-btn" :disabled="resyncing"
            @click.stop="resyncFlagged"
            v-tooltip.bottom="`Re-read physical size & timing from file for all ${flaggedUids.length} flagged image(s) — use if they were imported before this check existed and are actually fine.`">
            <i :class="['pi', resyncing ? 'pi-spin pi-spinner' : 'pi-sync']" />
          </button>
          <div class="resize-handle" @mousedown.stop.prevent="startResize('name', $event)" />
        </th>

        <!-- resizable: one column per channel index -->
        <th
          v-for="idx in channelIndices" :key="'ch-' + idx"
          class="col-resize col-ch-name"
          :style="{ width: colW('ch-' + idx), minWidth: colW('ch-' + idx) }"
          v-tooltip.bottom="`Channel ${idx} name`"
        >
          {{ idx }}
          <div class="resize-handle" @mousedown.stop.prevent="startResize('ch-' + idx, $event)" />
        </th>

        <!-- resizable: one column per attr key -->
        <th
          v-for="key in attrKeys" :key="'attr-' + key"
          class="col-resize col-attr"
          :style="{ width: colW('attr-' + key), minWidth: colW('attr-' + key) }"
          v-tooltip.bottom="`Attribute: ${key}`"
        >
          {{ key }}
          <div class="resize-handle" @mousedown.stop.prevent="startResize('attr-' + key, $event)" />
        </th>

        <!-- fixed: channel count (only when not showing per-channel columns) -->
        <th v-if="!showAttrs" class="col-fixed col-ch"
          v-tooltip.bottom="'Number of channels.'">Ch</th>

        <!-- fixed: z-slices (only when the set has a z-stack) -->
        <th v-if="anyZStack" class="col-fixed col-ch"
          v-tooltip.bottom="'Number of z-slices.'">Z</th>

        <!-- fixed: timelapse duration (only when the set has a timelapse) -->
        <th v-if="anyTimelapse" class="col-fixed col-dur"
          v-tooltip.bottom="'Total timelapse duration (first → last frame).'">Duration</th>

        <!-- fixed: per-module status -->
        <th v-if="module" class="col-fixed col-status">Status</th>

        <!-- fixed: delete -->
        <th v-if="allowDelete" class="col-fixed col-actions" />
      </tr>
    </thead>

    <tbody>
      <tr
        v-for="img in images" :key="img.uid"
        class="image-row"
        :class="{ 'row-selected': selected.has(img.uid), 'row-excluded': isExcluded(img) }"
        @click="toggle(img.uid)"
      >
        <td class="col-fixed col-check" @click.stop>
          <input type="checkbox" :checked="selected.has(img.uid)"
            :disabled="isExcluded(img) && !canSelectExcluded"
            @change="toggle(img.uid)"
            v-tooltip.right="isExcluded(img) && !canSelectExcluded ? 'Excluded — include it to select for a run.' : undefined" />
        </td>

        <td class="col-fixed col-viewer" @click.stop>
          <button
            class="viewer-btn"
            :class="{ 'viewer-active': project.napariImageUid === img.uid }"
            :disabled="napariLoading.has(img.uid)"
            @click="openInNapari(img.uid)"
            v-tooltip.right="project.napariImageUid === img.uid
              ? 'Currently shown in Napari — click to reload.'
              : 'Open this image in Napari viewer.'"
          >
            <i v-if="napariLoading.has(img.uid)" class="pi pi-spin pi-spinner" />
            <i v-else class="pi pi-eye" />
          </button>
        </td>

        <td class="col-resize td-name col-name">
          <span class="name-row">
            <button v-if="warnIconFor(img)" class="warn-icon-btn" @click.stop="physSizeDialogUid = img.uid"
              v-tooltip.right="warnIconFor(img)!.tip">
              <i class="pi pi-exclamation-triangle" />
            </button>
            <span v-if="qcFor(img)" class="qc-badge" :class="qcFor(img)!.level"
              v-tooltip.right="qcFor(img)!.long">
              <i class="pi pi-flag" /> QC
            </span>
            <span v-if="isExcluded(img)" class="excl-badge"
              v-tooltip.right="img.note ? `Excluded: ${img.note}` : 'Excluded from processing.'">
              <i class="pi pi-ban" /> Excluded
            </span>
            <span class="cell-text" v-tooltip.right="img.filepath ?? img.name">{{ img.name }}</span>
            <button v-if="pageIconFor()" class="row-icon-btn" @click.stop="physSizeDialogUid = img.uid"
              v-tooltip.right="pageIconFor()!.tip">
              <i class="pi pi-file-edit" />
            </button>
            <button class="row-icon-btn" @click.stop="copyUid(img.uid)"
              v-tooltip.right="copiedUid === img.uid ? 'Copied!' : 'Copy UID to clipboard'">
              <i :class="copiedUid === img.uid ? 'pi pi-check' : 'pi pi-copy'" />
            </button>
            <button class="row-icon-btn incl-toggle" @click.stop="setIncluded(img, isExcluded(img))"
              v-tooltip.right="isExcluded(img) ? 'Include in processing.' : 'Exclude from processing (won\'t be run).'">
              <i :class="isExcluded(img) ? 'pi pi-check-circle' : 'pi pi-ban'" />
            </button>
            <!-- run history: cog → popover listing the functions run on this image + when (provenance) -->
            <span class="runlog-cell" @click.stop>
              <button class="row-icon-btn runlog-cog" :class="{ on: runLogUid === img.uid }"
                @click.stop="toggleRunLog(img.uid, $event)"
                v-tooltip.right="'Functions run on this image'"><i class="pi pi-cog" /></button>
            </span>
          </span>
          <span class="uid-row">
            <span class="img-uid">{{ img.uid }}</span>
          </span>
          <!-- free-text note for ANY image (excluded or not) — for excluded images it doubles as the
               exclusion reason (shown in the badge tooltip + CSV) -->
          <span class="note-row" @click.stop>
            <input v-if="isEditing(img.uid, NOTE_KEY)"
              class="attr-edit" v-model="editValue" :ref="focusEditInput"
              :placeholder="isExcluded(img) ? 'reason (optional)' : 'note (optional)'"
              @keyup.enter="commitEdit(img.uid, NOTE_KEY, img.note ?? '', v => saveNote(img, v))"
              @keyup.esc="cancelEdit"
              @blur="commitEdit(img.uid, NOTE_KEY, img.note ?? '', v => saveNote(img, v))" />
            <span v-else class="note-text" @click="startEdit(img.uid, NOTE_KEY, img.note ?? '')"
              v-tooltip.right="'Click to edit the note.'">
              <i class="pi pi-comment" /> {{ img.note || 'add a note…' }}
            </span>
          </span>
        </td>

        <!-- channel names: editable only on the Metadata page (editableMeta); read-only elsewhere -->
        <td v-for="idx in channelIndices" :key="'ch-' + idx" class="col-resize">
          <template v-if="editableMeta && channelEditable(img, idx)">
            <input v-if="isEditing(img.uid, 'ch:' + idx)"
              class="attr-edit" v-model="editValue" :ref="focusEditInput" @click.stop
              @keyup.enter="commitEdit(img.uid, 'ch:' + idx, img.channelNames?.[idx - 1] ?? '', v => saveChannel(img, idx, v))"
              @keyup.esc="cancelEdit"
              @blur="commitEdit(img.uid, 'ch:' + idx, img.channelNames?.[idx - 1] ?? '', v => saveChannel(img, idx, v))" />
            <span v-else class="cell-text attr-cell"
              v-tooltip.right="img.channelNames?.[idx - 1] ? `${img.channelNames[idx - 1]} — click to edit` : `Name channel ${idx}`"
              @click.stop="startEdit(img.uid, 'ch:' + idx, img.channelNames?.[idx - 1] ?? '')">
              {{ img.channelNames?.[idx - 1] || '—' }}
            </span>
          </template>
          <span v-else-if="img.channelNames?.[idx - 1]" class="cell-text"
            v-tooltip.right="img.channelNames[idx - 1]">{{ img.channelNames[idx - 1] }}</span>
          <span v-else class="dim">—</span>
        </td>

        <!-- attributes: editable only on the Metadata page (editableMeta); read-only elsewhere -->
        <td v-for="key in attrKeys" :key="'attr-' + key" class="col-resize">
          <template v-if="editableMeta">
            <input v-if="isEditing(img.uid, 'attr:' + key)"
              class="attr-edit" v-model="editValue" :ref="focusEditInput" @click.stop
              @keyup.enter="commitEdit(img.uid, 'attr:' + key, img.attr?.[key] ?? '', v => saveAttr(img, key, v))"
              @keyup.esc="cancelEdit"
              @blur="commitEdit(img.uid, 'attr:' + key, img.attr?.[key] ?? '', v => saveAttr(img, key, v))" />
            <span v-else class="cell-text attr-cell"
              v-tooltip.right="img.attr?.[key] ? `${key}: ${img.attr[key]} — click to edit` : `Set ${key}`"
              @click.stop="startEdit(img.uid, 'attr:' + key, img.attr?.[key] ?? '')">
              {{ img.attr?.[key] || '—' }}
            </span>
          </template>
          <span v-else class="cell-text"
            v-tooltip.right="img.attr?.[key] ? `${key}: ${img.attr[key]}` : ''">{{ img.attr?.[key] || '—' }}</span>
        </td>

        <td v-if="!showAttrs" class="col-fixed col-ch">
          <span v-if="img.sizeC">{{ img.sizeC }}</span>
          <span v-else class="dim">—</span>
        </td>

        <td v-if="anyZStack" class="col-fixed col-ch">
          <span v-if="(img.sizeZ ?? 0) > 1">{{ img.sizeZ }}</span>
          <span v-else class="dim">—</span>
        </td>

        <td v-if="anyTimelapse" class="col-fixed col-dur">
          <span v-if="timelapseDuration(img.sizeT, img.timeIncrement, img.timeIncrementUnit)">{{
            timelapseDuration(img.sizeT, img.timeIncrement, img.timeIncrementUnit) }}</span>
          <span v-else class="dim">—</span>
        </td>

        <td v-if="module" class="col-fixed col-status">
          <span v-if="imageModuleStatus(img)"
            class="status-badge"
            :class="statusConfig[imageModuleStatus(img)!]?.cls"
            v-tooltip.right="statusConfig[imageModuleStatus(img)!]?.label">
            <span v-if="imageModuleStatus(img) === 'running'" class="spinner" />
            {{ statusConfig[imageModuleStatus(img)!]?.label }}
          </span>
        </td>

        <td v-if="allowDelete" class="col-fixed col-actions" @click.stop>
          <button
            class="action-btn del-btn"
            @click="confirmDelete(img.uid)"
            v-tooltip.left="'Remove this image from the set.'"
            :disabled="img.status === 'converting'"
          >
            <i class="pi pi-times" />
          </button>
        </td>
      </tr>
    </tbody>
  </table>
  </div>

  <PhysicalSizeDialog v-if="physSizeDialogUid"
    :set-uid="setUid" :focus-uid="physSizeDialogUid" :selected-uids="[...selected]"
    @close="physSizeDialogUid = null" />

  <!-- run-history popover — teleported to <body> so it escapes the table's scroll/transform
       containing block (was clipped by the following row); positioned (fixed) from the cog rect -->
  <Teleport to="body">
    <!-- carry the theme tokens (--cc-*) on the popover itself: teleported to <body> it's outside the
         shell's .cc-dark wrapper, so var(--cc-surface-1) etc. would otherwise be undefined (transparent) -->
    <div v-if="runLogImg" class="runlog-pop cc-dark" :style="runLogPos">
      <div class="runlog-hd">Run history</div>
      <div v-if="!runLogImg.runLog || !runLogImg.runLog.length" class="runlog-empty">No functions recorded yet.</div>
      <div v-for="(e, i) in [...(runLogImg.runLog ?? [])].reverse()" :key="i" class="runlog-row">
        <span class="runlog-fun">{{ e.fun }}</span>
        <span v-if="e.valueName" class="runlog-vn">{{ e.valueName }}</span>
        <span class="runlog-at">{{ fmtRunAt(e.at) }}</span>
      </div>
    </div>
  </Teleport>
</template>

<style scoped>
/* ── Layout ──────────────────────────────────────────────────────────────────── */

.delete-confirm {
  display: flex; align-items: center; gap: 0.6rem;
  padding: 0.5rem 1rem; background: #7f1d1d22;
  border-bottom: 1px solid #7f1d1d55; font-size: 0.82rem; flex-shrink: 0;
}

.empty-state {
  display: flex; flex-direction: column; align-items: center; justify-content: center;
  padding: 4rem 2rem; gap: 0.5rem; color: var(--cc-text-dim);
}
.empty-icon  { font-size: 2.5rem; margin-bottom: 0.5rem; opacity: 0.3; }
.empty-title { font-size: 0.95rem; font-weight: 600; color: var(--cc-text); margin: 0; }
.empty-hint  { font-size: 0.8rem; margin: 0; text-align: center; }

/* ── Table ───────────────────────────────────────────────────────────────────── */

.image-table {
  table-layout: fixed;
  width: max-content;
  min-width: 100%;
  border-collapse: collapse;
  font-size: 0.82rem;
}

.image-table thead th {
  text-align: left;
  font-size: 0.7rem; font-weight: 600;
  text-transform: uppercase; letter-spacing: 0.06em;
  color: var(--cc-text-dim);
  padding: 0.5rem 0.75rem;
  border-bottom: 1px solid var(--cc-border);
  white-space: nowrap;
  overflow: hidden;
}

.select-flagged-btn {
  background: none; border: none; cursor: pointer;
  color: var(--cc-text-dim); font-size: 0.72rem; padding: 0.1rem 0.3rem;
  margin-left: 0.3rem; border-radius: 0.2rem; vertical-align: middle;
}
.select-flagged-btn:hover { color: var(--cc-text); background: var(--cc-surface-2); }
.select-flagged-btn.active { color: #fbbf24; }
.select-flagged-btn.active:hover { color: #fcd34d; }
.select-flagged-btn:disabled { opacity: 0.5; cursor: not-allowed; }

.image-row { border-bottom: 1px solid var(--cc-border); cursor: pointer; transition: background 0.08s; }
.image-row:hover { background: var(--cc-surface-1); }
.image-row.row-selected { background: #a78bfa14; }
/* excluded: greyed but still visible (not hidden) — dim the whole row, un-dim a touch on hover so
   its note + include toggle stay usable */
.image-row.row-excluded { opacity: 0.5; cursor: default; }
.image-row.row-excluded:hover { opacity: 0.8; background: var(--cc-surface-1); }
.image-row td { padding: 0.4rem 0.75rem; vertical-align: middle; overflow: hidden; }

/* ── Column types ────────────────────────────────────────────────────────────── */

/* Fixed-width columns — not resizable */
.col-fixed  { flex-shrink: 0; }
.col-check  { width: 36px; min-width: 36px; }
.col-viewer { width: 32px; min-width: 32px; text-align: center; }
.col-ch     { width: 40px; min-width: 40px; text-align: center; color: var(--cc-text-dim); }
.col-dur    { width: 84px; min-width: 84px; text-align: center; color: var(--cc-text-dim); }
.col-status { width: 100px; min-width: 100px; }

/* ── Frozen left columns (Excel-style freeze panes) ────────────────────────────── */
/* The table scrolls horizontally inside .table-scroll; the checkbox + viewer + name columns stick to
   the left so the image identity stays visible. Each frozen cell needs an OPAQUE background (per row
   state, via --row-bg) so the scrolled columns pass UNDER it; the header sits above the body cells. */
.table-scroll { overflow-x: auto; width: 100%; }
.image-row { --row-bg: var(--cc-bg); }
.image-row:hover { --row-bg: var(--cc-surface-1); }
.image-row.row-selected { --row-bg: #1b1b29; }          /* ≈ the #a78bfa14 selected tint, opaque */
.image-table .col-check  { position: sticky; left: 0;    z-index: 2; }
.image-table .col-viewer { position: sticky; left: 36px; z-index: 2; }   /* after 36px checkbox */
.image-table .col-name   { position: sticky; left: 68px; z-index: 2; }   /* after 36+32px viewer */
.image-table tbody .col-check,
.image-table tbody .col-viewer,
.image-table tbody .col-name  { background: var(--row-bg); }
.image-table thead .col-check,
.image-table thead .col-viewer,
.image-table thead .col-name  { background: var(--cc-bg); z-index: 3; }  /* header above body */
.col-actions{ width: 36px; min-width: 36px; text-align: center; }
.col-ch-name { text-align: center; }

/* Resizable columns — width set dynamically via inline style */
.col-resize {
  position: relative;  /* needed for resize handle */
}

/* ── Resize handle ───────────────────────────────────────────────────────────── */

.resize-handle {
  position: absolute;
  right: 0; top: 0;
  width: 5px; height: 100%;
  cursor: col-resize;
  z-index: 1;
}
.resize-handle::after {
  content: '';
  position: absolute;
  right: 1px; top: 20%; bottom: 20%;
  width: 1px;
  background: var(--cc-border);
  opacity: 0;
  transition: opacity 0.15s;
}
th:hover .resize-handle::after { opacity: 1; }

/* ── Cell content ────────────────────────────────────────────────────────────── */

.cell-text {
  display: block;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  color: var(--cc-text);
}

.td-name .cell-text { color: var(--cc-text); }

.name-row { display: flex; align-items: center; gap: 0.3rem; min-width: 0; }
.name-row .cell-text { flex: 1; min-width: 0; }

/* always visible when flagged — impossible-to-miss, sits in front of the name */
.warn-icon-btn {
  flex-shrink: 0; background: none; border: none; cursor: pointer;
  color: #fbbf24; font-size: 0.75rem; padding: 0.1rem; line-height: 1;
}
.warn-icon-btn:hover { color: #fcd34d; }

/* QC badge — advisory "output looks off" flag; distinct from the metadata warning icon. */
.qc-badge {
  flex-shrink: 0; display: inline-flex; align-items: center; gap: 0.2rem;
  font-size: 0.6rem; font-weight: 700; letter-spacing: 0.04em;
  padding: 0.05rem 0.3rem; border-radius: 0.25rem; cursor: help;
  border: 1px solid transparent;
}
.qc-badge .pi { font-size: 0.62rem; }
.qc-badge.warn { color: #fbbf24; background: #7c2d1233; border-color: #f59e0b55; }
.qc-badge.info { color: var(--cc-text-dim); background: var(--cc-surface-2); border-color: var(--cc-border); }

/* excluded badge — persistent "this image is excluded" pill; carries the note as its tooltip */
.excl-badge {
  flex-shrink: 0; display: inline-flex; align-items: center; gap: 0.2rem;
  font-size: 0.6rem; font-weight: 700; letter-spacing: 0.04em;
  padding: 0.05rem 0.3rem; border-radius: 0.25rem; cursor: help;
  color: #fca5a5; background: #7f1d1d33; border: 1px solid #7f1d1d66;
}
.excl-badge .pi { font-size: 0.62rem; }

/* include/exclude toggle: hidden until row hover like the other row actions, but ALWAYS visible on
   an excluded row so there's an obvious way back */
.image-row.row-excluded .incl-toggle { opacity: 1; }
.incl-toggle:hover { color: #fca5a5; }

/* exclusion note — editable reason under the uid, only on excluded rows */
.note-row { display: flex; align-items: center; margin-top: 0.15rem; }
.note-text {
  font-size: 0.68rem; color: var(--cc-text-dim); cursor: text;
  display: inline-flex; align-items: center; gap: 0.25rem;
  border-radius: 3px; padding: 0 2px;
}
.note-text:hover { background: var(--cc-surface-2); outline: 1px dashed var(--cc-border); }
.note-text .pi { font-size: 0.62rem; }

/* row-hover actions after the name: the "open editor" page icon + copy-UID — same look, one class */
.row-icon-btn {
  flex-shrink: 0; background: none; border: none; cursor: pointer;
  color: var(--cc-text-dim); font-size: 0.72rem; padding: 0.1rem 0.2rem;
  border-radius: 0.2rem; line-height: 1;
  opacity: 0; transition: opacity 0.1s, color 0.1s, background 0.1s;
}
.image-row:hover .row-icon-btn { opacity: 1; }
.row-icon-btn:hover { color: var(--cc-text); background: var(--cc-surface-2); }

/* editable attribute cell: click to edit; subtle hover affordance */
.attr-cell { cursor: text; border-radius: 3px; padding: 0 2px; }
.attr-cell:hover { background: var(--cc-surface-2); outline: 1px dashed var(--cc-border); }
.attr-edit { width: 100%; box-sizing: border-box; font: inherit; padding: 1px 3px;
  border: 1px solid var(--cc-accent); border-radius: 3px; background: var(--cc-surface-1); color: var(--cc-text); }

.uid-row {
  display: flex;
  align-items: center;
  gap: 0.25rem;
  min-width: 0;
}
/* run-history cog + popover (fixed so it escapes the table's horizontal scroll) */
.runlog-cell { position: relative; display: inline-flex; flex-shrink: 0; }
.runlog-cog.on { color: var(--cc-text); background: var(--cc-surface-2); opacity: 1; }
.runlog-pop { z-index: 40; min-width: 15rem; max-height: 16rem; overflow-y: auto;
  padding: 6px 8px; background: var(--cc-surface-1); border: 1px solid var(--cc-border);
  border-radius: 5px; box-shadow: 0 6px 18px rgba(0, 0, 0, 0.4); }
.runlog-hd { font-size: 0.6rem; text-transform: uppercase; letter-spacing: 0.05em; color: var(--cc-text-dim); margin-bottom: 4px; }
.runlog-empty { font-size: 0.7rem; color: var(--cc-text-dim); }
.runlog-row { display: flex; align-items: baseline; gap: 6px; padding: 2px 0; font-size: 0.7rem; }
.runlog-fun { font-weight: 600; color: var(--cc-text); font-family: var(--cc-mono); }
.runlog-vn { color: var(--cc-accent); font-size: 0.62rem; }
.runlog-at { margin-left: auto; color: var(--cc-text-dim); font-variant-numeric: tabular-nums; white-space: nowrap; }
.img-uid {
  font-family: var(--cc-mono);
  font-size: 0.68rem;
  color: var(--cc-text-dim);
  letter-spacing: 0.03em;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  flex: 1;
  min-width: 0;
}

.dim { color: var(--cc-text-dim); }

/* ── Status badge ────────────────────────────────────────────────────────────── */

.status-badge {
  display: inline-flex; align-items: center; gap: 0.35rem;
  font-size: 0.7rem; font-weight: 600; padding: 0.15rem 0.5rem;
  border-radius: 999px; text-transform: uppercase; letter-spacing: 0.04em;
}
.st-pending  { background: #27272a;    color: #71717a; }
.st-queued   { background: #2d1b69;    color: #c4b5fd; }
.st-running  { background: #1e3a5f;    color: #93c5fd; }
.st-done     { background: #14532d44;  color: #86efac; }
.st-failed   { background: #7f1d1d44;  color: #fca5a5; }

.spinner {
  width: 7px; height: 7px; border-radius: 50%;
  border: 1.5px solid #93c5fd44; border-top-color: #93c5fd;
  animation: spin 0.7s linear infinite; flex-shrink: 0;
}
@keyframes spin { to { transform: rotate(360deg); } }

/* ── Napari eye button ───────────────────────────────────────────────────────── */

.viewer-btn {
  background: none; border: none; cursor: pointer;
  color: var(--cc-text-dim); font-size: 0.8rem;
  padding: 0.2rem 0.3rem; border-radius: 0.25rem;
  opacity: 0; transition: opacity 0.12s, color 0.12s, background 0.12s; line-height: 1;
}
.image-row:hover .viewer-btn { opacity: 0.6; }
.viewer-btn:hover { opacity: 1 !important; color: #93c5fd; background: #1e3a5f44; }
.viewer-btn:disabled { opacity: 0.2 !important; cursor: not-allowed; }
.viewer-active { opacity: 1 !important; color: #f97316; }
.viewer-active:hover { color: #fb923c; background: #f9731622; }

/* ── Delete button ───────────────────────────────────────────────────────────── */

.action-btn {
  background: none; border: none; cursor: pointer;
  color: var(--cc-text-dim); font-size: 0.75rem;
  padding: 0.2rem 0.4rem; border-radius: 0.25rem;
  opacity: 0; transition: opacity 0.1s, background 0.1s;
}
.image-row:hover .action-btn { opacity: 1; }
.action-btn:disabled { opacity: 0.25 !important; cursor: not-allowed; }
.del-btn:hover { background: #7f1d1d55; color: #fca5a5; }

/* ── Shared buttons ──────────────────────────────────────────────────────────── */

.btn-sm {
  display: flex; align-items: center; gap: 0.3rem;
  font-size: 0.78rem; font-weight: 500; padding: 0.3rem 0.65rem;
  border-radius: 0.35rem; border: 1px solid transparent; cursor: pointer;
}
.btn-ghost  { background: var(--cc-surface-2); border-color: var(--cc-border); color: var(--cc-text-dim); }
.btn-ghost:hover { color: var(--cc-text); }
.btn-danger { background: #7f1d1d44; border-color: #7f1d1d; color: #fca5a5; }
.btn-danger:hover { background: #7f1d1d88; }
</style>
