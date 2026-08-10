<script setup lang="ts">
// Per-project notebook registry table (Phase 3). Mirrors ImageTable's inline-edit pattern for the
// description field. Project notebooks are managed (create/describe/snapshot/delete); shipped
// examples are read-only (duplicate-into-project only). See docs/todo/NOTEBOOK_PLAYGROUND_PLAN.md.
import { ref, onMounted, onUnmounted, watch } from 'vue'
import { useLogStore } from '../stores/log'
import { useInlineEdit } from '../composables/useInlineEdit'
import { useWsStore } from '../stores/ws'
import ConfirmDeleteButton from './ConfirmDeleteButton.vue'
import SelectionTable, { type SelectionColumn } from './SelectionTable.vue'

const props = defineProps<{
  projectUid: string
  serverUrl: string
  serverSecret: string
  serverRunning: boolean
}>()

const log = useLogStore()

interface Notebook {
  name: string; file: string; scope: 'project' | 'example'
  path: string; description: string; version: number
}
const notebooks = ref<Notebook[]>([])
// Every cell is rendered by a `#cell-` slot (an icon, an inline edit, a badge), so these name the
// HEADERS and say what each sorts by. `versionText` is a display string; the raw `version` sorts it.
//
// The widths are starting points for the drag-resize path (`column-width-key` below): a version and a
// scope badge need a fraction of what a name does, and one width for all four is what pushes a table
// off its page. Description is given the largest share because it is the one that overflows — under
// `table-layout: fixed` the leftover width is split across the declared ones, so it also grows most.
const NB_COLUMNS: SelectionColumn[] = [
  { key: 'name',        label: 'Name',        sortable: true, width: 200 },
  { key: 'description', label: 'Description', sortable: true, width: 280 },
  { key: 'versionText', label: 'Ver',         sortable: true, sortKey: 'version', width: 60 },
  { key: 'scope',       label: 'Source',      sortable: true, width: 90 },
]
const loading = ref(false)
const newName = ref('')
const busy = ref(false)

async function post(path: string, body: Record<string, unknown>) {
  const res = await fetch(path, {
    method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body),
  })
  const d = await res.json().catch(() => ({}))
  if (!res.ok) throw new Error(d.error ?? `HTTP ${res.status}`)
  return d
}

async function refresh() {
  if (!props.projectUid) return
  loading.value = true
  try {
    const res = await fetch(`/api/notebooks?projectUid=${encodeURIComponent(props.projectUid)}`)
    const d = await res.json()
    if (!res.ok) throw new Error(d.error ?? `HTTP ${res.status}`)
    notebooks.value = d.notebooks ?? []
  } catch (e) {
    log.error(`Failed to list notebooks: ${e instanceof Error ? e.message : String(e)}`, { source: 'notebooks' })
  } finally {
    loading.value = false
  }
}

async function createNotebook() {
  const name = newName.value.trim()
  if (!name || busy.value) return
  busy.value = true
  try {
    await post('/api/notebooks/create', { projectUid: props.projectUid, name })
    newName.value = ''
    await refresh()
    log.info(`Created notebook "${name}".`, { source: 'notebooks' })
  } catch (e) {
    log.error(`Create failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'notebooks' })
  } finally {
    busy.value = false
  }
}

async function duplicate(nb: Notebook) {
  busy.value = true
  try {
    const d = await post('/api/notebooks/duplicate', { projectUid: props.projectUid, file: nb.file, scope: nb.scope })
    await refresh()
    log.info(`Duplicated to "${d.file}".`, { source: 'notebooks' })
  } catch (e) {
    log.error(`Duplicate failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'notebooks' })
  } finally {
    busy.value = false
  }
}

async function snapshot(nb: Notebook) {
  busy.value = true
  try {
    const d = await post('/api/notebooks/snapshot', { projectUid: props.projectUid, file: nb.file })
    await refresh()
    if (expandedFile.value === nb.file) await loadSnapshots(nb.file)
    log.info(`Snapshot ${d.snapshot} saved (now v${d.version}).`, { source: 'notebooks' })
  } catch (e) {
    log.error(`Snapshot failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'notebooks' })
  } finally {
    busy.value = false
  }
}

// Version history / restore.
interface Snapshot { version: number; file: string }
const expandedFile = ref<string | null>(null)
const snapshots = ref<Snapshot[]>([])
const snapsLoading = ref(false)
const restoreVersion = ref<number | null>(null)   // the version chosen in the dropdown

async function loadSnapshots(file: string) {
  snapsLoading.value = true
  confirmingRestore.value = null
  confirmingPrune.value = null
  try {
    const res = await fetch(`/api/notebooks/snapshots?projectUid=${encodeURIComponent(props.projectUid)}&file=${encodeURIComponent(file)}`)
    const d = await res.json()
    if (!res.ok) throw new Error(d.error ?? `HTTP ${res.status}`)
    snapshots.value = d.snapshots ?? []
    restoreVersion.value = snapshots.value.length ? snapshots.value[0].version : null   // default: newest
  } catch (e) {
    log.error(`Failed to load history: ${e instanceof Error ? e.message : String(e)}`, { source: 'notebooks' })
    snapshots.value = []
    restoreVersion.value = null
  } finally {
    snapsLoading.value = false
  }
}

function toggleHistory(nb: Notebook) {
  if (expandedFile.value === nb.file) { expandedFile.value = null; return }
  expandedFile.value = nb.file
  loadSnapshots(nb.file)
}

// Two-click confirm (guards un-snapshotted edits; restore does NOT auto-snapshot).
const confirmingRestore = ref<string | null>(null)
async function restore(nb: Notebook) {
  const version = restoreVersion.value
  if (version == null) return
  confirmingPrune.value = null
  if (confirmingRestore.value !== nb.file) { confirmingRestore.value = nb.file; return }
  confirmingRestore.value = null
  busy.value = true
  try {
    await post('/api/notebooks/restore', { projectUid: props.projectUid, file: nb.file, version, force: true })
    await refresh()
    if (expandedFile.value === nb.file) await loadSnapshots(nb.file)
    log.info(`Restored ${nb.file} to v${version}.`, { source: 'notebooks' })
  } catch (e) {
    log.error(`Restore failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'notebooks' })
  } finally {
    busy.value = false
  }
}

// Prune: keep only the current version's snapshot, delete the older ones. Two-click confirm (destructive
// of history). Prunes to nb.version (the live/current version), independent of the restore dropdown.
// The description is a per-notebook field and is never touched by prune.
const confirmingPrune = ref<string | null>(null)
async function prune(nb: Notebook) {
  confirmingRestore.value = null
  if (confirmingPrune.value !== nb.file) { confirmingPrune.value = nb.file; return }
  confirmingPrune.value = null
  busy.value = true
  try {
    const d = await post('/api/notebooks/prune', { projectUid: props.projectUid, file: nb.file })
    await refresh()
    if (expandedFile.value === nb.file) await loadSnapshots(nb.file)
    const n = d.removed?.length ?? 0
    log.info(`Pruned ${nb.file} — kept v${d.kept}, removed ${n} older snapshot${n !== 1 ? 's' : ''}.`, { source: 'notebooks' })
  } catch (e) {
    log.error(`Prune failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'notebooks' })
  } finally {
    busy.value = false
  }
}

// Two-click delete confirm (no modal dependency).
async function remove(nb: Notebook) {   // confirmation handled by ConfirmDeleteButton
  busy.value = true
  try {
    // force: the two-click confirm IS the user's confirmation; satisfies the server-running guard.
    await post('/api/notebooks/delete', { projectUid: props.projectUid, file: nb.file, force: true })
    await refresh()
    log.info(`Deleted notebook "${nb.file}".`, { source: 'notebooks' })
  } catch (e) {
    log.error(`Delete failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'notebooks' })
  } finally {
    busy.value = false
  }
}

// Inline description edit (mirrors ImageTable startEdit/commitEdit).
// edit-in-place, shared with the tables and the canvas managers (composables/useInlineEdit)
const { draft: editValue, isEditing, start, cancel: cancelEdit, commit,
        focusInput: focusEditInput } = useInlineEdit()

function startEdit(nb: Notebook) {
  if (nb.scope !== 'project') return          // examples are read-only
  start(nb.file, nb.description)
}
// a description MAY be cleared, so the empty case is a real save here
const commitEdit = (nb: Notebook) => commit(nb.file, nb.description, async val => {
  try {
    await post('/api/notebooks/describe', { projectUid: props.projectUid, file: nb.file, description: val })
    nb.description = val
  } catch (e) {
    log.error(`Save description failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'notebooks' })
  }
})

function openUrl(nb: Notebook) {
  const s = props.serverSecret ? `&secret=${encodeURIComponent(props.serverSecret)}` : ''
  return `${props.serverUrl}open?path=${encodeURIComponent(nb.path)}${s}`
}

onMounted(refresh)
watch(() => props.projectUid, refresh)

// Auto-refresh when a notebook is created out-of-band (e.g. Claude's create_notebook) for THIS project.
const ws = useWsStore()
function onNotebooksChanged(d: any) { if (String(d?.projectUid ?? '') === props.projectUid) refresh() }
onMounted(() => ws.on('notebooks_changed', onNotebooksChanged))
onUnmounted(() => ws.off('notebooks_changed', onNotebooksChanged))
defineExpose({ refresh })
</script>

<template>
  <div class="nbt">
    <div class="nbt-add">
      <input v-model="newName" type="text" placeholder="New notebook name…"
             v-tooltip.right="'Name for the new notebook'" @keyup.enter="createNotebook" :disabled="busy" />
      <button class="cc-btn cc-btn-primary" :disabled="busy || !newName.trim()" @click="createNotebook">
        <i class="pi pi-plus" /> Add notebook
      </button>
      <button class="cc-btn cc-btn-ghost" :disabled="loading" @click="refresh">
        <i class="pi pi-refresh" /> Refresh
      </button>
    </div>

    <!-- The canonical table (docs/UI.md): `none` — a notebook row isn't "selected", the buttons act.
         The description cell and the version-history panel come in through `#cell-description` and
         `#row-detail`, so this file no longer carries a <thead>/<tbody> of its own.

         `column-width-key` turns on the sized path: drag a header's right edge to widen a column, and
         the reset-widths button (beside "Name") puts them back — both are the table's, and the widths
         persist per user. `actions-width` MUST be declared with it: fixed layout gives the trailing
         column only what the others leave over, and this row carries five controls. -->
    <div class="nbt-scroll">
    <SelectionTable class="nbt-table" selection-mode="none" :columns="NB_COLUMNS" :rows="notebooks"
                    id-key="file" sort-storage-key="cc.notebooks.sort" actions-label="Actions"
                    column-width-key="cc.notebooks.colw" actions-width="11rem" fit="content"
                    :row-tooltip="nb => nb.scope === 'project' ? nb.file : `${nb.file} — shipped example, read-only`"
                    :is-expanded="nb => expandedFile === nb.file">
      <template #cell-name="{ row: nb }">
        <span class="nbt-name"><i class="pi pi-file" /> {{ nb.name }}</span>
      </template>

      <!-- Description: inline-editable for project notebooks -->
      <template #cell-description="{ row: nb }">
        <span class="nbt-desc">
          <input v-if="isEditing(nb.file)" :ref="focusEditInput" v-model="editValue" v-tooltip.right="'Enter to save, Esc to cancel'"
                 type="text" @blur="commitEdit(nb)" @keyup.enter="commitEdit(nb)" @keyup.esc="cancelEdit" />
          <span v-else :class="{ 'nbt-editable': nb.scope === 'project', 'nbt-muted': !nb.description }"
                @click="startEdit(nb)">
            {{ nb.description || (nb.scope === 'project' ? 'Add a description…' : '—') }}
          </span>
        </span>
      </template>

      <template #cell-versionText="{ row: nb }">
        <span class="nbt-ver">{{ nb.scope === 'project' && nb.version ? `v${nb.version}` : '—' }}</span>
      </template>
      <template #cell-scope="{ row: nb }">
        <span class="nb-badge" :class="`scope-${nb.scope}`">{{ nb.scope }}</span>
      </template>

      <template #actions="{ row: nb }">
            <a v-if="serverRunning" class="cc-btn cc-btn-ghost" :href="openUrl(nb)" target="_blank"
               rel="noopener" v-tooltip.top="'Open in Pluto'"><i class="pi pi-external-link" /></a>
            <button v-else class="cc-btn cc-btn-ghost" disabled
                    v-tooltip.top="'Start the server first'"><i class="pi pi-external-link" /></button>

            <button class="cc-btn cc-btn-ghost" :disabled="busy" @click="duplicate(nb)"
                    v-tooltip.top="'Duplicate into this project'"><i class="pi pi-copy" /></button>

            <button v-if="nb.scope === 'project'" class="cc-btn cc-btn-ghost" :disabled="busy"
                    @click="snapshot(nb)" v-tooltip.top="'Snapshot this version (provenance)'">
              <i class="pi pi-camera" />
            </button>

            <button v-if="nb.scope === 'project'" class="cc-btn cc-btn-ghost"
                    :class="{ 'nbt-active': expandedFile === nb.file }" :disabled="busy"
                    @click="toggleHistory(nb)" v-tooltip.top="'Version history / restore'">
              <i class="pi pi-history" />
            </button>

            <ConfirmDeleteButton v-if="nb.scope === 'project'" :disabled="busy" title="Delete"
                    :armed-title="serverRunning ? 'Server running — close this notebook in Pluto first, then click to confirm' : 'Click again to confirm'"
                    @confirm="remove(nb)" />
      </template>

      <!-- Version history / restore panel. The TABLE renders the row; WHICH notebook is open stays
           this component's state (`expandedFile`), so nothing about expansion moved. -->
      <template #row-detail="{ row: nb }">
        <div class="nbt-history cc-row">
              <span v-if="snapsLoading" class="nbt-muted">Loading history…</span>
              <span v-else-if="!snapshots.length" class="nbt-muted">
                No snapshots yet — click <i class="pi pi-camera" /> to freeze this version.
              </span>
              <template v-else>
                <label class="nbt-hist-label">Restore to version</label>
                <select v-model.number="restoreVersion" :disabled="busy" v-tooltip.top="'Snapshot to roll this notebook back to'"
                        @change="confirmingRestore = null">
                  <option v-for="s in snapshots" :key="s.version" :value="s.version">v{{ s.version }}</option>
                </select>
                <button class="cc-btn" :class="confirmingRestore === nb.file ? 'cc-btn-primary' : 'cc-btn-ghost'"
                        :disabled="busy || restoreVersion == null" @click="restore(nb)"
                        v-tooltip.top="confirmingRestore === nb.file
                          ? (serverRunning ? 'Server running — close this notebook in Pluto first, then confirm' : 'Click Confirm to overwrite the current notebook')
                          : 'Overwrites the current notebook — snapshot first if you want to keep it'">
                  <i class="pi pi-replay" /> {{ confirmingRestore === nb.file ? 'Confirm restore' : 'Restore' }}
                </button>
                <span class="nbt-hist-sep" />
                <button v-if="snapshots.length > 1" class="cc-btn"
                        :class="confirmingPrune === nb.file ? 'cc-btn-primary' : 'cc-btn-ghost'"
                        :disabled="busy" @click="prune(nb)"
                        v-tooltip.top="confirmingPrune === nb.file
                          ? `Click Confirm to delete every snapshot except the current (v${nb.version})`
                          : `Keep only the current version (v${nb.version}) — delete older snapshots`">
                  <i class="pi pi-filter" /> {{ confirmingPrune === nb.file ? 'Confirm prune' : 'Prune' }}
                </button>
          </template>
        </div>
      </template>

      <template #empty>
        <span v-if="!loading" class="cc-muted">No notebooks yet — add one, or duplicate an example.</span>
      </template>
    </SelectionTable>
    </div>
  </div>
</template>

<style scoped>
.nbt-add { display: flex; align-items: center; gap: .5rem; margin-bottom: .75rem; }
.nbt-add input { flex: 0 1 240px; }
/* header, borders, padding, hover and the empty row are SelectionTable's now. The font size is this
   table's own — a notebook list is read, not scanned for numbers. */
/* the sizing is the table's (`fit="content"`); the font size is this one's own */
.nbt-table { font-size: var(--cc-fs-lg); }
/* The TABLE scrolls, not the page: a column dragged wider than the page would otherwise push a
   horizontal scrollbar onto the whole document (same wrapper as the movie list). */
.nbt-scroll { overflow-x: auto; }
.nbt-name { white-space: nowrap; }
.nbt-desc input { width: 100%; }
.nbt-editable { cursor: text; }
.nbt-muted { color: var(--cc-text-dim); font-style: italic; }
.nbt-ver { white-space: nowrap; color: var(--cc-text-dim); }
/* The actions CELL is SelectionTable's `.sel-actions` now (right-aligned, nowrap, and deliberately
   not `display:flex`, which would take the <td> out of the table layout). What stays is only the
   denser button padding this table wants — five controls per row is more than most. */
.nbt-table :deep(.sel-actions) .cc-btn { padding: .25rem .45rem; }
.nbt-danger { color: #f85149; }
.nbt-active { color: #58a6ff; }
.nbt-table :deep(.sel-detail-row) td { background: var(--cc-surface-2, rgba(255,255,255,0.03)); }
.nbt-history { font-size: var(--cc-fs-md); }
.nbt-hist-label { color: var(--cc-text-dim); }
.nbt-hist-sep { flex: 1 1 auto; }   /* push Prune to the far end, away from the Restore control */
.nbt-snap { display: inline-flex; align-items: center; gap: .25rem; border: 1px solid var(--cc-border); border-radius: var(--cc-radius-md); padding: .1rem .1rem .1rem .5rem; }
.nbt-snap-ver { font-variant-numeric: tabular-nums; }
.nbt-snap .cc-btn { padding: .15rem .4rem; }
.nb-badge { font-size: var(--cc-fs-sm); padding: .1rem .45rem; border-radius: var(--cc-radius-pill); border: 1px solid var(--cc-border); }
.nb-badge.scope-project { color: #58a6ff; border-color: #58a6ff55; }
.nb-badge.scope-example { color: #888; }
</style>
