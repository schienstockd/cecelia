<!--
  Project manager modal — create new projects and open recent ones.
  Opens from the project block in AppSidebar.
-->
<script setup lang="ts">
import { ref, watch, computed, onMounted } from 'vue'
import BaseModal from './BaseModal.vue'
import ConfirmDeleteButton from './ConfirmDeleteButton.vue'
import SelectionTable, { type SelectionColumn } from './SelectionTable.vue'
import FileBrowser from './FileBrowser.vue'
import CollapsibleSection from './CollapsibleSection.vue'
import CcProgressBar from './CcProgressBar.vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useWsStore } from '../stores/ws'
import { useTaskStore } from '../stores/tasks'
import { runningTaskCount } from '../utils/runningTasks'

const emit = defineEmits<{ (e: 'close'): void }>()

const projectMeta = useProjectMetaStore()
const ws = useWsStore()
const taskStore = useTaskStore()

type Tab = 'recent' | 'new'
const tab = ref<Tab>('recent')

// ── New project form ────────────────────────────────────────────────────────
const newName = ref('')
const nameError = ref('')

async function createProject() {
  nameError.value = ''
  // First-ever project: don't block on naming — fall back to a default (onboarding). Later projects
  // still require an explicit name. See docs/todo/ONBOARDING_PLAN.md (P3).
  const name = newName.value.trim() || (projectMeta.recent.length === 0 ? 'My first project' : '')
  if (!name) { nameError.value = 'Name is required.'; return }
  const ok = await projectMeta.createProject(name)
  if (ok) emit('close')
}

// ── Recent list ─────────────────────────────────────────────────────────────
const selectedUid = ref<string | null>(null)
// SelectionTable's model is a plain id; this panel's is nullable (nothing selected on an empty list)
const selectedUidModel = computed<string>({
  get: () => selectedUid.value ?? '',
  set: v => { selectedUid.value = v || null },
})

// The table renders display strings and sorts on the raw value beside them (docs/UI.md), so the
// truncated path and the formatted date each carry what they mean.
const PROJECT_COLUMNS: SelectionColumn[] = [
  { key: 'name',     label: 'Name',        sortable: true },
  { key: 'pathText', label: 'Location',    sortable: true, sortKey: 'path' },
  { key: 'dateText', label: 'Last opened', sortable: true, sortKey: 'lastOpenedAt' },
]
const projectRows = computed(() => projectMeta.recent.map(p => ({
  ...p,
  pathText: p.path.length > 40 ? '…' + p.path.slice(-38) : p.path,
  dateText: formatDate(p.lastOpenedAt),
})))

async function openSelected() {
  if (!selectedUid.value) return
  const ok = await projectMeta.openProject(selectedUid.value)
  if (ok) emit('close')
}

// Permanently delete a project from disk (armed via ConfirmDeleteButton). Never offered for the
// currently-open project. Clears the selection if the deleted one was selected.
async function deleteProject(uid: string) {
  await projectMeta.deleteProject(uid)
  if (selectedUid.value === uid) selectedUid.value = projectMeta.recent[0]?.uid ?? null
}

// ── Export / import (background jobs over the WS task rail — see docs/JOBS.md) ─────────────────
// Neither needs an open project: export reads a project dir off disk by uid; import creates a new one.
// One I/O op runs at a time; its progress shows inline here and in the global Tasks list.
const ioTaskId  = ref<string | null>(null)
const importPath = ref('')
const ioTask = computed(() => ioTaskId.value ? taskStore.tasks.find(t => t.id === ioTaskId.value) ?? null : null)
const ioBusy = computed(() => !!ioTask.value && (ioTask.value.status === 'running' || ioTask.value.status === 'queued'))

// Bundles already sitting in the default export folder — the import picker lists these so the user can
// pick an exported bundle instead of typing a path (a pasted path still works as a fallback).
interface BundleInfo { uid: string; name: string; path: string; stores: number }
const bundles = ref<BundleInfo[]>([])
const exportDir = ref('')   // destination for exports; default filled from /api/projects/bundles
async function fetchBundles() {
  try {
    const res = await fetch('/api/projects/bundles')
    if (res.ok) {
      const body = await res.json() as { bundles?: BundleInfo[]; exportDir?: string }
      bundles.value = body.bundles ?? []
      if (!exportDir.value && body.exportDir) exportDir.value = body.exportDir   // default, user can change
    }
  } catch { /* export dir may not exist yet */ }
}
function pickBundle(path: string) { if (path) importPath.value = path }

// Server-side folder/bundle browser (reused FileBrowser) — so export destination and import source can
// be ANY server-accessible path (mounts/network drives included), not just the auto-discovered folder.
const browserMode = ref<'export' | 'import' | null>(null)
function onBrowserSelect(paths: string[]) {
  const p = paths[0]
  if (p) { if (browserMode.value === 'export') exportDir.value = p; else importPath.value = p }
  browserMode.value = null
}

// Warn before exporting while analysis tasks are in flight — packing a store that's being written can
// capture a torn snapshot. The count comes from the shared `runningTaskCount` (utils/runningTasks.ts),
// which quit uses too — same question, one asker.
const exportWarn = ref<{ p: { uid: string; name: string }; count: number } | null>(null)
async function exportProject(p: { uid: string; name: string }) {
  if (ioBusy.value) return
  const n = await runningTaskCount()
  if (n > 0) { exportWarn.value = { p, count: n }; return }
  doExport(p)
}
function doExport(p: { uid: string; name: string }) {
  if (ioBusy.value) return
  exportWarn.value = null
  const outDir = exportDir.value || undefined
  const entry = taskStore.add({
    module: 'project', label: `Export ${p.name}`, imageUid: '', imageName: '', status: 'queued',
    taskName: 'export', funName: 'project.export', params: { outDir }, projectUid: p.uid, startedAt: new Date(),
  })
  ioTaskId.value = entry.id
  ws.send({ type: 'project:export', taskId: entry.id, projectUid: p.uid, outDir })
}

// Import: first peek the bundle — if its project uid already exists, ask what to do; otherwise just go.
interface Conflict { path: string; uid: string; name: string }
const conflict = ref<Conflict | null>(null)

async function importBundle() {
  const bundle = importPath.value.trim()
  if (!bundle || ioBusy.value) return
  try {
    const res = await fetch(`/api/projects/bundle-info?path=${encodeURIComponent(bundle)}`)
    if (res.ok) {
      const info = await res.json() as { uid: string; name: string; exists: boolean }
      if (info.exists) { conflict.value = { path: bundle, uid: info.uid, name: info.name }; return }
    }
  } catch { /* fall through — backend still refuses on a real collision */ }
  doImport(bundle)
}

function doImport(bundle: string, mode?: 'replace' | 'copy') {
  if (ioBusy.value) return
  conflict.value = null
  const entry = taskStore.add({
    module: 'project', label: `Import ${bundle.split('/').pop()}`, imageUid: '', imageName: '',
    status: 'queued', taskName: 'import', funName: 'project.import', params: { bundle, mode },
    projectUid: '', startedAt: new Date(),
  })
  ioTaskId.value = entry.id
  ws.send({ type: 'project:import', taskId: entry.id, bundle, mode })
}

function cancelIo() {
  if (ioTaskId.value) ws.send({ type: 'task:cancel', taskId: ioTaskId.value })
}

// On completion: a finished import means a new project on disk (refresh the list); a finished export
// means a new bundle in the export folder (refresh the picker).
watch(() => ioTask.value?.status, (s) => {
  if (s !== 'done') return
  if (ioTask.value?.funName === 'project.import') { projectMeta.fetchRecent(); importPath.value = '' }
  if (ioTask.value?.funName === 'project.export') fetchBundles()
})

onMounted(async () => {
  fetchBundles()
  // Re-attach to an export/import already running — users will start one, close the manager, and
  // reopen. The job runs server-side regardless; rebind so its progress shows here again. (Newest
  // first in the store, so the first match is the current run.)
  const running = taskStore.tasks.find(t =>
    (t.funName === 'project.export' || t.funName === 'project.import') &&
    (t.status === 'running' || t.status === 'queued'))
  if (running) ioTaskId.value = running.id
  await projectMeta.fetchRecent()
  if (projectMeta.recent.length === 0) {
    tab.value = 'new'
    newName.value = 'My first project'   // first-project pre-fill; user can rename (P3)
  } else {
    // Auto-select the most recent project (first in the sorted list)
    selectedUid.value = projectMeta.current?.uid ?? projectMeta.recent[0].uid
  }
})

// Keep selection in sync if the list reloads
watch(() => projectMeta.recent, (list) => {
  if (list.length > 0 && !selectedUid.value) {
    selectedUid.value = projectMeta.current?.uid ?? list[0].uid
  }
})

function formatDate(iso: string | null): string {
  if (!iso) return '—'
  try { return new Date(iso).toLocaleDateString(undefined, { year: 'numeric', month: 'short', day: 'numeric' }) }
  catch { return iso }
}

</script>

<template>
  <BaseModal title="Project manager" icon="pi-folder-open" width="700px" @close="$emit('close')">

    <!-- tabs -->
    <template #toolbar>
      <div class="pp-tabs">
        <button
          class="pp-tab"
          :class="{ active: tab === 'recent' }"
          @click="tab = 'recent'"
          v-tooltip.bottom="'Browse recently opened and created projects'"
        >
          <i class="pi pi-history" /> Recent
        </button>
        <button
          class="pp-tab"
          :class="{ active: tab === 'new' }"
          @click="tab = 'new'"
          v-tooltip.bottom="'Create a new Cecelia project in a local folder'"
        >
          <i class="pi pi-plus" /> New project
        </button>
      </div>
    </template>

      <!-- ── RECENT tab ─────────────────────────────────────────────────── -->
      <div v-if="tab === 'recent'" class="pp-body">

        <div v-if="projectMeta.recent.length === 0" class="pp-empty cc-empty">
          <i class="pi pi-folder" style="font-size:2rem; opacity:0.2" />
          <p>No projects yet.<br>A project holds all your images and analysis for one experiment.</p>
          <button class="cc-btn cc-btn-ghost" @click="tab = 'new'">
            <i class="pi pi-plus" /> Create your first project
          </button>
        </div>

        <!-- The canonical table (docs/UI.md). It was hand-rolled here before `SelectionTable` could do
             single-select with row actions; the sortable headers come for free with the move. -->
        <SelectionTable v-else class="proj-table" :columns="PROJECT_COLUMNS" :rows="projectRows"
                        v-model="selectedUidModel" id-key="uid" sort-storage-key="cc.projects.sort"
                        :row-class="p => ({ active: projectMeta.current?.uid === p.uid })"
                        :row-tooltip="p => p.uid === projectMeta.current?.uid
                          ? 'This project is already open'
                          : `Double-click to open ${p.name}`"
                        @row-dblclick="openSelected">
          <template #cell-name="{ row: p }">
            <span class="proj-name">{{ p.name }}</span>
            <span v-if="projectMeta.current?.uid === p.uid" class="open-badge"
              v-tooltip.right="'Currently open project'">open</span>
          </template>
          <template #cell-pathText="{ row: p }">
            <span class="dim cc-muted" v-tooltip.bottom="p.path">{{ p.pathText }}</span>
          </template>
          <template #actions="{ row: p }">
            <!-- export to a portable .ccbundle (allowed for any project, incl. the open one) -->
            <button class="pp-row-btn cc-btn cc-btn-bare cc-btn-icon" :disabled="ioBusy"
                    @click="exportProject(p)"
                    v-tooltip.left="'Export this project to a portable .ccbundle'">
              <i class="pi pi-download" />
            </button>
            <!-- delete a project (not the open one) — canonical arm→confirm single button -->
            <ConfirmDeleteButton v-if="projectMeta.current?.uid !== p.uid"
                                 title="Delete this project from disk"
                                 armed-title="Click again to permanently delete"
                                 @confirm="deleteProject(p.uid)" />
          </template>
        </SelectionTable>
      </div>

      <!-- ── NEW PROJECT tab ───────────────────────────────────────────── -->
      <div v-if="tab === 'new'" class="pp-body pp-form">

        <div class="form-row">
          <label class="form-label"
            v-tooltip.right="'Name for this project; need not match the folder name'">
            Project name
          </label>
          <input
            class="form-input"
            :class="{ 'input-error': nameError }"
            v-model="newName"
            placeholder="e.g. Tumour microenvironment 2025"
            @keyup.enter="createProject"
            v-tooltip.right="'Give the project a unique, descriptive name'"
          />
          <span class="field-error" v-if="nameError">{{ nameError }}</span>
        </div>

        <div class="form-row">
          <span class="field-hint cc-muted"
            v-tooltip.right="'Override with CECELIA_PROJECTS_DIR'">
            <i class="pi pi-folder" />
            <template v-if="projectMeta.projectsDir">
              Project will be created in
              <code class="dir-hint">{{ projectMeta.projectsDir }}</code>
            </template>
            <template v-else>
              Start the Julia server to see the projects directory.
            </template>
          </span>
        </div>

      </div>

      <!-- footer -->
      <div class="pp-footer">
        <button class="cc-btn cc-btn-ghost" @click="$emit('close')"
          v-tooltip.top="'Close without changes'">
          Cancel
        </button>

        <template v-if="tab === 'recent'">
          <button
            class="cc-btn cc-btn-primary"
            :disabled="!selectedUid || projectMeta.loading || selectedUid === projectMeta.current?.uid"
            @click="openSelected"
            v-tooltip.top="selectedUid && selectedUid !== projectMeta.current?.uid
              ? 'Open the selected project'
              : 'Select a project from the list above'">
            <i class="pi pi-folder-open" />
            Open project
          </button>
        </template>

        <template v-if="tab === 'new'">
          <button
            class="cc-btn cc-btn-primary"
            :disabled="projectMeta.loading"
            @click="createProject"
            v-tooltip.top="'Create the project and open it'">
            <i class="pi pi-plus" v-if="!projectMeta.loading" />
            <i class="pi pi-spin pi-spinner" v-else />
            Create project
          </button>
        </template>
      </div>

      <!-- Export / import — a secondary utility, so it lives in a collapsed section BELOW the primary
           list + Open project action (rather than wedged between them). Recent tab only. The live
           job status sits OUTSIDE the collapsible so an in-flight export (started from a row button)
           or import stays visible even when the controls are collapsed. -->
      <template v-if="tab === 'recent'">
        <div v-if="ioTask" class="pp-io pp-io-live">
          <div class="pp-io-status cc-row cc-muted" :class="ioTask.status">
            <span class="pp-io-label">{{ ioTask.label }}</span>
            <span class="pp-io-state">{{ ioTask.status }}</span>
            <CcProgressBar v-if="ioBusy" class="pp-io-bar" size="bar"
              :value="ioTask.progress" :aria-label="`${ioTask.label} progress`" />
            <button v-if="ioBusy" class="pp-row-btn cc-btn cc-btn-bare cc-btn-icon" @click="cancelIo" v-tooltip.top="'Cancel'">
              <i class="pi pi-times" />
            </button>
            <span v-if="ioTask.log.length" class="pp-io-log">{{ ioTask.log[ioTask.log.length - 1] }}</span>
          </div>
        </div>

        <CollapsibleSection label="Export / import project" :default-open="false"
                            storage-key="cc-pp-io-open" max-height="none">
          <div class="pp-io">
            <div class="pp-io-dest cc-row">
              <span class="dim cc-muted">Exports to</span>
              <code class="pp-io-destpath" v-tooltip.top="exportDir">{{ exportDir || 'cecelia_exports (default)' }}</code>
              <button class="cc-btn cc-btn-ghost" :disabled="ioBusy" @click="browserMode = 'export'"
                      v-tooltip.top="'Select where exported bundles are written (any folder, incl. mounted servers/drives)'">
                <i class="pi pi-folder-open" /> Change
              </button>
            </div>
            <p class="pp-io-hint dim cc-muted">Export a project to a portable <code>.ccbundle</code> with the
              <i class="pi pi-download" /> button on any row above. Import one below.</p>
            <div class="pp-io-import cc-row">
              <select v-if="bundles.length" class="form-input pp-io-select" :disabled="ioBusy"
                      @change="pickBundle(($event.target as HTMLSelectElement).value)"
                      v-tooltip.top="'Select a bundle from the export folder'">
                <option value="">Select an exported bundle…</option>
                <option v-for="b in bundles" :key="b.path" :value="b.path">
                  {{ b.name || b.uid }} — {{ b.stores }} store{{ b.stores === 1 ? '' : 's' }}
                </option>
              </select>
              <input class="form-input pp-io-path" v-model="importPath" :disabled="ioBusy"
                     :placeholder="bundles.length ? '…or paste / browse to a .ccbundle path' : 'Paste or browse to a .ccbundle folder…'"
                     @keyup.enter="importBundle"
                     v-tooltip.top="'Absolute path to a .ccbundle folder produced by Export'" />
              <button class="cc-btn cc-btn-ghost" :disabled="ioBusy" @click="browserMode = 'import'"
                      v-tooltip.top="'Browse for a .ccbundle folder anywhere (incl. mounted servers/drives)'">
                <i class="pi pi-folder-open" /> Browse
              </button>
              <button class="cc-btn cc-btn-ghost" :disabled="!importPath.trim() || ioBusy" @click="importBundle"
                      v-tooltip.top="'Import a project from a .ccbundle folder'">
                <i class="pi pi-upload" /> Import
              </button>
            </div>
          </div>
        </CollapsibleSection>
      </template>

  </BaseModal>

  <!-- server-side picker: export destination (dir) or import source (.ccbundle) — any path, incl. mounts -->
  <FileBrowser v-if="browserMode"
    :mode="browserMode === 'export' ? 'dir' : 'bundle'"
    @select="onBrowserSelect" @close="browserMode = null" />

  <!-- import collision: the bundle's project already exists → replace / copy / cancel -->
  <BaseModal v-if="conflict" title="Project already exists" icon="pi-exclamation-triangle"
             width="480px" @close="conflict = null">
    <div class="pp-conflict">
      <p>A project <strong>{{ conflict.name }}</strong> (<code>{{ conflict.uid }}</code>) already
        exists on disk. What would you like to do?</p>
      <ul class="pp-conflict-opts cc-muted cc-fs-md">
        <li><strong>Import as copy</strong> — keep both; the import gets a new id and its name is suffixed.</li>
        <li><strong>Replace</strong> — overwrite the existing project with the bundle. <em>Destructive.</em></li>
      </ul>
      <p class="pp-danger-note"><i class="pi pi-exclamation-triangle" /> <strong>Replace</strong>
        permanently deletes the existing project's data and cannot be undone — at your own risk.</p>
    </div>
    <template #footer>
      <button class="cc-btn cc-btn-ghost" @click="conflict = null"
              v-tooltip.top="'Do nothing — keep the existing project'">Cancel</button>
      <button class="cc-btn cc-btn-danger" :disabled="projectMeta.current?.uid === conflict.uid"
              @click="doImport(conflict!.path, 'replace')"
              v-tooltip.top="projectMeta.current?.uid === conflict.uid
                ? 'Close the project first — can\'t replace the one that\'s open'
                : 'Overwrite the existing project — destructive, cannot be undone'">
        <i class="pi pi-exclamation-triangle" /> Replace (at your own risk)
      </button>
      <button class="cc-btn cc-btn-primary" @click="doImport(conflict!.path, 'copy')"
              v-tooltip.top="'Import as a new project (new id) — keeps both'">
        <i class="pi pi-copy" /> Import as copy
      </button>
    </template>
  </BaseModal>

  <!-- warn: analysis tasks in flight when exporting → possible torn snapshot -->
  <BaseModal v-if="exportWarn" title="Tasks are still running" icon="pi-exclamation-triangle"
             width="460px" @close="exportWarn = null">
    <div class="pp-conflict">
      <p>You have <strong>{{ exportWarn.count }}</strong> task{{ exportWarn.count === 1 ? '' : 's' }}
        running. Exporting <strong>{{ exportWarn.p.name }}</strong> now can capture an inconsistent
        snapshot of a store that's being written. Best to wait until they finish.</p>
    </div>
    <template #footer>
      <button class="cc-btn cc-btn-primary" @click="exportWarn = null"
              v-tooltip.top="'Wait for the running tasks to finish'">Wait</button>
      <button class="cc-btn cc-btn-ghost" @click="doExport(exportWarn!.p)"
              v-tooltip.top="'Export now despite the running tasks'">Export anyway</button>
    </template>
  </BaseModal>
</template>

<style scoped>
/* Shell (overlay/box/header) lives in BaseModal; only panel-specific styles remain here. */

/* tabs */
.pp-tabs {
  display: flex;
  border-bottom: 1px solid var(--cc-border);
  flex-shrink: 0;
}
.pp-tab {
  background: none; border: none; cursor: pointer;
  font-size: var(--cc-fs-md); font-weight: 500;
  color: var(--cc-text-dim);
  padding: 0.55rem 1rem;
  display: flex; align-items: center; gap: 0.4rem;
  border-bottom: 2px solid transparent;
  transition: color 0.1s, border-color 0.1s;
}
.pp-tab:hover { color: var(--cc-text); }
.pp-tab.active { color: var(--cc-accent); border-bottom-color: var(--cc-accent); }

/* body — BaseModal's cc-modal-body owns the scroll; the tab panes are plain flow. */
.pp-body { display: flex; flex-direction: column; }

.pp-empty { gap: 0.75rem; padding: 3rem 1rem; }
.pp-empty p { margin: 0; }

/* project table — the header, row, hover, selected and cell padding are all SelectionTable's now.
   What is left is only what this panel means: the already-open project reads as a tint. */
.proj-table { width: 100%; }

/* small square row-action button (export, cancel) — matches ConfirmDeleteButton's footprint */
.pp-row-btn { transition: color 0.1s, background 0.1s; }   /* + cc-btn cc-btn-bare cc-btn-icon */
.pp-row-btn:hover:not(:disabled) { color: var(--cc-accent); background: var(--cc-surface-2); }
.pp-row-btn:disabled { opacity: 0.3; cursor: not-allowed; }

/* import bar + active export/import status */
.pp-io {
  display: flex; flex-direction: column; gap: 0.5rem;
  padding: 0.75rem;
}
/* live export/import progress — standalone below the footer, so it needs its own top separator */
.pp-io-live { border-top: 1px solid var(--cc-border); }
.pp-io-hint { margin: 0; }   /* + .cc-muted (size + colour) */
.pp-io-hint .pi { font-size: var(--cc-fs-xs); }
.pp-conflict { padding: 1rem 1.25rem; font-size: var(--cc-fs-md); color: var(--cc-text); }
.pp-conflict p { margin: 0 0 0.6rem; }
.pp-conflict code { font-family: var(--cc-mono); font-size: var(--cc-fs-sm); background: var(--cc-surface-2); padding: 0.05rem 0.3rem; border-radius: var(--cc-radius-xs); }
.pp-conflict-opts { margin: 0; padding-left: 1.1rem; }
.pp-conflict-opts li { margin: 0.2rem 0; }

.pp-io-dest { font-size: var(--cc-fs-sm); }
.pp-io-destpath {
  font-family: var(--cc-mono); font-size: var(--cc-fs-xs); color: var(--cc-text);
  background: var(--cc-surface-2); padding: 0.1rem 0.35rem; border-radius: var(--cc-radius-xs);
  max-width: 60%; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
  margin-right: auto;   /* push the Change button to the row's right edge (aligns with Import below) */
}

.pp-io-select { flex: 1 1 180px; }
.pp-io-path { flex: 2 1 180px; }

.pp-io-label { color: var(--cc-text); font-weight: 500; }
.pp-io-state { text-transform: uppercase; font-size: var(--cc-fs-2xs); font-weight: 700; letter-spacing: 0.05em; }
.pp-io-status.done  .pp-io-state { color: #34d399; }
.pp-io-status.failed .pp-io-state, .pp-io-status.cancelled .pp-io-state { color: #fca5a5; }
.pp-io-bar { flex: 0 0 90px; }   /* geometry only — the bar itself is CcProgressBar */
.pp-io-log {
  flex: 1 1 100%; min-width: 0;
  font-family: var(--cc-mono); font-size: var(--cc-fs-xs); opacity: 0.75;
  white-space: normal; word-break: break-all; user-select: text;   /* show full path, selectable */
}

/* The ALREADY-OPEN project — a fainter tint than the selection, which SelectionTable draws. Scoped
   styles carry the component's data attribute, and the row is SelectionTable's element, so this needs
   `:deep` to reach it. */
:deep(.active) { background: #a78bfa0a; }

.proj-name { color: var(--cc-text); font-weight: 500; margin-right: 0.4rem; }
.open-badge {
  font-size: var(--cc-fs-2xs); font-weight: 700; text-transform: uppercase;
  padding: 0.05rem 0.35rem; border-radius: var(--cc-radius-xs);
  background: #a78bfa22; color: var(--cc-accent);
  border: 1px solid #a78bfa44;
}
/* form */
.pp-form { padding: 1.25rem 1.5rem; display: flex; flex-direction: column; gap: 1.25rem; }

.form-row { display: flex; flex-direction: column; gap: 0.35rem; }
.form-label {
  font-size: var(--cc-fs-sm); font-weight: 600;
  color: var(--cc-text); cursor: default;
}
/* visual styling from the global form base (style.css) */
.form-input.input-error { border-color: var(--cc-sev-fail); }
.form-input::placeholder { color: var(--cc-text-dim); }

.field-error { font-size: var(--cc-fs-sm); color: #fca5a5; }
.field-hint { display: flex; align-items: center; gap: 0.3rem; }
.dir-hint {
  font-family: var(--cc-mono);
  font-size: var(--cc-fs-xs);
  background: var(--cc-surface-2);
  padding: 0.1rem 0.35rem;
  border-radius: var(--cc-radius-xs);
  color: var(--cc-text);
}

/* footer */
.pp-footer {
  display: flex; align-items: center; justify-content: flex-end; gap: 0.4rem;
  padding: 0.65rem 1rem;
  border-top: 1px solid var(--cc-border);
  background: var(--cc-surface-1);
  flex-shrink: 0;
}

/* buttons use the global .cc-btn utilities (style.css) */
.pp-danger-note {
  color: #fca5a5; font-size: var(--cc-fs-sm); display: flex; gap: 0.4rem; align-items: flex-start;
  margin-top: 0.6rem; padding: 0.4rem 0.55rem; border-radius: var(--cc-radius-sm);
  background: #b91c1c1a; border: 1px solid #b91c1c44;
}
</style>
