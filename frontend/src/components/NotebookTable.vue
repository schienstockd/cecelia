<script setup lang="ts">
// Per-project notebook registry table (Phase 3). Mirrors ImageTable's inline-edit pattern for the
// description field. Project notebooks are managed (create/describe/snapshot/delete); shipped
// examples are read-only (duplicate-into-project only). See docs/todo/NOTEBOOK_PLAYGROUND_PLAN.md.
import { ref, onMounted, watch } from 'vue'
import { useLogStore } from '../stores/log'

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

// Two-click delete confirm (no modal dependency).
const confirmingDelete = ref<string | null>(null)
async function remove(nb: Notebook) {
  if (confirmingDelete.value !== nb.file) { confirmingDelete.value = nb.file; return }
  confirmingDelete.value = null
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
const editingFile = ref<string | null>(null)
const editValue = ref('')
function startEdit(nb: Notebook) {
  if (nb.scope !== 'project') return          // examples are read-only
  editingFile.value = nb.file
  editValue.value = nb.description
}
function cancelEdit() { editingFile.value = null }
function focusEditInput(el: unknown) {
  const i = el as HTMLInputElement | null
  if (i && i !== document.activeElement) i.focus()
}
async function commitEdit(nb: Notebook) {
  if (editingFile.value !== nb.file) return
  editingFile.value = null
  const val = editValue.value.trim()
  if ((nb.description ?? '') === val) return
  try {
    await post('/api/notebooks/describe', { projectUid: props.projectUid, file: nb.file, description: val })
    nb.description = val
  } catch (e) {
    log.error(`Save description failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'notebooks' })
  }
}

function openUrl(nb: Notebook) {
  const s = props.serverSecret ? `&secret=${encodeURIComponent(props.serverSecret)}` : ''
  return `${props.serverUrl}open?path=${encodeURIComponent(nb.path)}${s}`
}

onMounted(refresh)
watch(() => props.projectUid, refresh)
defineExpose({ refresh })
</script>

<template>
  <div class="nbt">
    <div class="nbt-add">
      <input v-model="newName" type="text" placeholder="New notebook name…"
             @keyup.enter="createNotebook" :disabled="busy" />
      <button class="cc-btn cc-btn-primary" :disabled="busy || !newName.trim()" @click="createNotebook">
        <i class="pi pi-plus" /> Add notebook
      </button>
      <button class="cc-btn cc-btn-ghost" :disabled="loading" @click="refresh">
        <i class="pi pi-refresh" /> Refresh
      </button>
    </div>

    <table class="nbt-table">
      <thead>
        <tr><th>Name</th><th>Description</th><th>Ver</th><th>Source</th><th class="nbt-actions-h">Actions</th></tr>
      </thead>
      <tbody>
        <template v-for="nb in notebooks" :key="`${nb.scope}/${nb.file}`">
        <tr>
          <td class="nbt-name"><i class="pi pi-file" /> {{ nb.name }}</td>

          <!-- Description: inline-editable for project notebooks -->
          <td class="nbt-desc">
            <input v-if="editingFile === nb.file" :ref="focusEditInput" v-model="editValue"
                   type="text" @blur="commitEdit(nb)" @keyup.enter="commitEdit(nb)" @keyup.esc="cancelEdit" />
            <span v-else :class="{ 'nbt-editable': nb.scope === 'project', 'nbt-muted': !nb.description }"
                  @click="startEdit(nb)">
              {{ nb.description || (nb.scope === 'project' ? 'Add a description…' : '—') }}
            </span>
          </td>

          <td class="nbt-ver">{{ nb.scope === 'project' && nb.version ? `v${nb.version}` : '—' }}</td>
          <td>
            <span class="nb-badge" :class="`scope-${nb.scope}`">{{ nb.scope }}</span>
          </td>

          <td class="nbt-actions">
            <a v-if="serverRunning" class="cc-btn cc-btn-ghost" :href="openUrl(nb)" target="_blank"
               rel="noopener" v-tooltip.top="'Open in Pluto'"><i class="pi pi-external-link" /></a>
            <button v-else class="cc-btn cc-btn-ghost" disabled
                    v-tooltip.top="'Launch the server first'"><i class="pi pi-external-link" /></button>

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

            <button v-if="nb.scope === 'project'"
                    class="cc-btn cc-btn-ghost" :class="{ 'nbt-danger': confirmingDelete === nb.file }"
                    :disabled="busy" @click="remove(nb)"
                    v-tooltip.top="confirmingDelete === nb.file
                      ? (serverRunning ? 'Server running — close this notebook in Pluto first, then click to confirm' : 'Click again to confirm')
                      : 'Delete'">
              <i class="pi" :class="confirmingDelete === nb.file ? 'pi-check' : 'pi-trash'" />
            </button>
          </td>
        </tr>

        <!-- Version history / restore panel -->
        <tr v-if="expandedFile === nb.file" class="nbt-history-row">
          <td colspan="5">
            <div class="nbt-history">
              <span v-if="snapsLoading" class="nbt-muted">Loading history…</span>
              <span v-else-if="!snapshots.length" class="nbt-muted">
                No snapshots yet — click <i class="pi pi-camera" /> to freeze this version.
              </span>
              <template v-else>
                <label class="nbt-hist-label">Restore to version</label>
                <select v-model.number="restoreVersion" :disabled="busy"
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
              </template>
            </div>
          </td>
        </tr>
        </template>
        <tr v-if="!notebooks.length && !loading">
          <td colspan="5" class="nbt-empty">No notebooks yet — add one, or duplicate an example.</td>
        </tr>
      </tbody>
    </table>
  </div>
</template>

<style scoped>
.nbt-add { display: flex; align-items: center; gap: .5rem; margin-bottom: .75rem; }
.nbt-add input { flex: 0 1 240px; }
.nbt-table { width: 100%; border-collapse: collapse; font-size: .9rem; }
.nbt-table th, .nbt-table td { text-align: left; padding: .45rem .6rem; border-bottom: 1px solid var(--cc-border, #2a2a2a); }
.nbt-table th { color: var(--cc-text-muted, #888); font-weight: 600; }
.nbt-name { white-space: nowrap; }
.nbt-desc input { width: 100%; }
.nbt-editable { cursor: text; }
.nbt-muted { color: var(--cc-text-muted, #888); font-style: italic; }
.nbt-ver { white-space: nowrap; color: var(--cc-text-muted, #888); }
.nbt-actions-h { text-align: right; }
.nbt-actions { display: flex; gap: .3rem; justify-content: flex-end; }
.nbt-actions .cc-btn { padding: .25rem .45rem; }
.nbt-danger { color: #f85149; }
.nbt-active { color: #58a6ff; }
.nbt-empty { color: var(--cc-text-muted, #888); text-align: center; padding: 1rem; }
.nbt-history-row td { background: var(--cc-surface-2, rgba(255,255,255,0.03)); }
.nbt-history { display: flex; align-items: center; flex-wrap: wrap; gap: .5rem; font-size: .85rem; }
.nbt-hist-label { color: var(--cc-text-muted, #888); }
.nbt-snap { display: inline-flex; align-items: center; gap: .25rem; border: 1px solid var(--cc-border, #333); border-radius: 6px; padding: .1rem .1rem .1rem .5rem; }
.nbt-snap-ver { font-variant-numeric: tabular-nums; }
.nbt-snap .cc-btn { padding: .15rem .4rem; }
.nb-badge { font-size: .72rem; padding: .1rem .45rem; border-radius: 999px; border: 1px solid var(--cc-border, #333); }
.nb-badge.scope-project { color: #58a6ff; border-color: #58a6ff55; }
.nb-badge.scope-example { color: #888; }
</style>
