<script setup lang="ts">
import { ref, computed, watch, onMounted, onUnmounted } from 'vue'
import { useProjectStore, type CciaImage } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useLogStore } from '../stores/log'
import { useTaskStore, type TaskStatus } from '../stores/tasks'
import { useSettingsStore } from '../stores/settings'

const props = defineProps<{
  setUid: string
  module?: string      // when provided, shows a per-module status column
  showAttrs?: boolean  // show per-channel + attr columns
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
const settings    = useSettingsStore()

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

// ── Inline attribute editing (click an attribute cell to edit its value) ────────
const editingCell = ref<string | null>(null)        // `${uid}:${key}` currently being edited
const editValue = ref('')
const cellKey = (uid: string, key: string) => `${uid}:${key}`
function startAttrEdit(img: CciaImage, key: string) {
  editingCell.value = cellKey(img.uid, key)
  editValue.value = img.attr?.[key] ?? ''
}
function cancelAttrEdit() { editingCell.value = null }
// Focus the attr-edit input when it mounts, without stealing focus if it already has it.
// (Lives here, not in the template, because the template scope doesn't expose `document`.)
function focusAttrInput(el: unknown) {
  const i = el as HTMLInputElement | null
  if (i && i !== document.activeElement) i.focus()
}
async function commitAttrEdit(img: CciaImage, key: string) {
  if (editingCell.value !== cellKey(img.uid, key)) return
  editingCell.value = null
  const projectUid = projectMeta.current?.uid
  const val = editValue.value.trim()
  if (!projectUid || (img.attr?.[key] ?? '') === val) return    // nothing to do
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
  let uids = imgs.length ? stored.filter(u => imgs.some(i => i.uid === u)) : stored
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

const allSelected = computed(() =>
  images.value.length > 0 && selected.value.size === images.value.length
)
const someSelected = computed(() =>
  selected.value.size > 0 && selected.value.size < images.value.length
)

function toggleAll() {
  selected.value = selected.value.size === images.value.length
    ? new Set()
    : new Set(images.value.map(i => i.uid))
  commit()
}

function toggle(uid: string) {
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

async function openInNapari(imageUid: string) {
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  // clicking the eye on the ALREADY-open image = reload it. Delegate to ViewerPanel (it owns the
  // overlay logic), which reloads DATA only unless the user ticked reset — no needless pyramid reopen.
  if (project.napariImageUid === imageUid) { project.requestNapariReload(); return }
  napariLoading.value = new Set([...napariLoading.value, imageUid])
  const autoProps = settings.napariAutoSaveLayerProps
  try {
    const res = await fetch('/api/napari/open', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        projectUid,
        imageUid,
        autoSaveProps: autoProps,
        autoLoadProps: autoProps,
        show3D:        settings.napariShow3D,
        asDask:        settings.napariAsDask,
      }),
    })
    const body = await res.json().catch(() => ({})) as { ok?: boolean; starting?: boolean; message?: string; error?: string }
    if (res.status === 202 && body.starting) {
      log.info(body.message ?? 'Napari is starting…', { source: 'viewer' }); return
    }
    if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
    log.info('Opened image in Napari.', { source: 'viewer' })
  } catch (e) {
    log.error(`Napari: ${e instanceof Error ? e.message : String(e)}`, { source: 'viewer' })
  } finally {
    napariLoading.value = new Set([...napariLoading.value].filter(u => u !== imageUid))
  }
}

// ── Status ────────────────────────────────────────────────────────────────────

function imageModuleStatus(img: CciaImage): TaskStatus | 'pending' | null {
  if (!props.module) return null
  const t = taskStore.forModule(props.module).find(t => t.imageUid === img.uid)
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

  <table v-else class="image-table">
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

        <!-- resizable: name -->
        <th class="col-resize" :style="{ width: colW('name'), minWidth: colW('name') }">
          Name
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
        :class="{ 'row-selected': selected.has(img.uid) }"
        @click="toggle(img.uid)"
      >
        <td class="col-fixed col-check" @click.stop>
          <input type="checkbox" :checked="selected.has(img.uid)" @change="toggle(img.uid)" />
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

        <td class="col-resize td-name">
          <span class="cell-text" v-tooltip.right="img.filepath ?? img.name">{{ img.name }}</span>
          <span class="uid-row">
            <span class="img-uid">{{ img.uid }}</span>
            <button class="uid-copy" @click.stop="copyUid(img.uid)"
              v-tooltip.right="copiedUid === img.uid ? 'Copied!' : 'Copy UID to clipboard'">
              <i :class="copiedUid === img.uid ? 'pi pi-check' : 'pi pi-copy'" />
            </button>
          </span>
        </td>

        <td v-for="idx in channelIndices" :key="'ch-' + idx" class="col-resize">
          <span v-if="img.channelNames?.[idx - 1]"
            class="cell-text"
            v-tooltip.right="img.channelNames[idx - 1]">
            {{ img.channelNames[idx - 1] }}
          </span>
          <span v-else class="dim">—</span>
        </td>

        <td v-for="key in attrKeys" :key="'attr-' + key" class="col-resize">
          <input v-if="editingCell === cellKey(img.uid, key)"
            class="attr-edit" v-model="editValue"
            :ref="focusAttrInput"
            @keyup.enter="commitAttrEdit(img, key)" @keyup.esc="cancelAttrEdit"
            @blur="commitAttrEdit(img, key)" />
          <span v-else class="cell-text attr-cell"
            v-tooltip.right="img.attr?.[key] ? `${key}: ${img.attr[key]} — click to edit` : `Set ${key}`"
            @click="startAttrEdit(img, key)">
            {{ img.attr?.[key] || '—' }}
          </span>
        </td>

        <td v-if="!showAttrs" class="col-fixed col-ch">
          <span v-if="img.sizeC">{{ img.sizeC }}</span>
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

.image-row { border-bottom: 1px solid var(--cc-border); cursor: pointer; transition: background 0.08s; }
.image-row:hover { background: var(--cc-surface-1); }
.image-row.row-selected { background: #a78bfa14; }
.image-row td { padding: 0.4rem 0.75rem; vertical-align: middle; overflow: hidden; }

/* ── Column types ────────────────────────────────────────────────────────────── */

/* Fixed-width columns — not resizable */
.col-fixed  { flex-shrink: 0; }
.col-check  { width: 36px; min-width: 36px; }
.col-viewer { width: 32px; min-width: 32px; text-align: center; }
.col-ch     { width: 40px; min-width: 40px; text-align: center; color: var(--cc-text-dim); }
.col-status { width: 100px; min-width: 100px; }
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
.uid-copy {
  flex-shrink: 0;
  background: none;
  border: none;
  cursor: pointer;
  color: var(--cc-text-dim);
  font-size: 0.65rem;
  padding: 0.1rem 0.2rem;
  border-radius: 0.2rem;
  line-height: 1;
  opacity: 0;
  transition: opacity 0.1s;
}
tr:hover .uid-copy { opacity: 1; }
.uid-copy:hover { color: var(--cc-text); background: var(--cc-surface-2); }

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
