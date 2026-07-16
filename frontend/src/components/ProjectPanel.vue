<!--
  Project manager modal — create new projects and open recent ones.
  Opens from the project block in AppSidebar.
-->
<script setup lang="ts">
import { ref, watch, onMounted } from 'vue'
import BaseModal from './BaseModal.vue'
import { useProjectMetaStore, type ProjectType } from '../stores/projectMeta'

const emit = defineEmits<{ (e: 'close'): void }>()

const projectMeta = useProjectMetaStore()

type Tab = 'recent' | 'new'
const tab = ref<Tab>('recent')

// ── New project form ────────────────────────────────────────────────────────
const newName = ref('')
const newType = ref<ProjectType>('static')
const nameError = ref('')

const typeOptions: { value: ProjectType; label: string; tip: string }[] = [
  { value: 'static',  label: 'Static',   tip: 'Fixed tissue — confocal, slide scanner, etc.' },
  { value: 'live',    label: 'Live',      tip: 'Time-lapse imaging with cell tracking.' },
  { value: 'flow',    label: 'Flow',      tip: 'Flow or mass cytometry (no image data).' },
]

async function createProject() {
  nameError.value = ''
  // First-ever project: don't block on naming — fall back to a default (onboarding). Later projects
  // still require an explicit name. See docs/todo/ONBOARDING_PLAN.md (P3).
  const name = newName.value.trim() || (projectMeta.recent.length === 0 ? 'My first project' : '')
  if (!name) { nameError.value = 'Name is required.'; return }
  const ok = await projectMeta.createProject(name, newType.value)
  if (ok) emit('close')
}

// ── Recent list ─────────────────────────────────────────────────────────────
const selectedUid = ref<string | null>(null)

async function openSelected() {
  if (!selectedUid.value) return
  const ok = await projectMeta.openProject(selectedUid.value)
  if (ok) emit('close')
}

onMounted(async () => {
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

const typeColour: Record<ProjectType, string> = {
  static: '#a78bfa',
  live:   '#34d399',
  flow:   '#60a5fa',
}
</script>

<template>
  <BaseModal title="Project Manager" icon="pi-folder-open" width="700px" @close="$emit('close')">

    <!-- tabs -->
    <template #toolbar>
      <div class="pp-tabs">
        <button
          class="pp-tab"
          :class="{ active: tab === 'recent' }"
          @click="tab = 'recent'"
          v-tooltip.bottom="'Browse recently opened and created projects.'"
        >
          <i class="pi pi-history" /> Recent
        </button>
        <button
          class="pp-tab"
          :class="{ active: tab === 'new' }"
          @click="tab = 'new'"
          v-tooltip.bottom="'Create a new Cecelia project in a local folder.'"
        >
          <i class="pi pi-plus" /> New project
        </button>
      </div>
    </template>

      <!-- ── RECENT tab ─────────────────────────────────────────────────── -->
      <div v-if="tab === 'recent'" class="pp-body">

        <div v-if="projectMeta.recent.length === 0" class="pp-empty">
          <i class="pi pi-folder" style="font-size:2rem; opacity:0.2" />
          <p>No projects yet.<br>A project holds all your images and analysis for one experiment.</p>
          <button class="btn-ghost btn-sm" @click="tab = 'new'">
            <i class="pi pi-plus" /> Create your first project
          </button>
        </div>

        <table v-else class="proj-table">
          <thead>
            <tr>
              <th class="col-sel" />
              <th class="col-name">Name</th>
              <th class="col-type">Type</th>
              <th class="col-path">Location</th>
              <th class="col-date">Last opened</th>
            </tr>
          </thead>
          <tbody>
            <tr
              v-for="p in projectMeta.recent"
              :key="p.uid"
              class="proj-row"
              :class="{ selected: selectedUid === p.uid, active: projectMeta.current?.uid === p.uid }"
              @click="selectedUid = p.uid"
              @dblclick="openSelected"
              v-tooltip.right="p.uid === projectMeta.current?.uid
                ? 'This project is already open.'
                : `Double-click to open ${p.name}`"
            >
              <td class="col-sel">
                <input type="radio"
                  :checked="selectedUid === p.uid"
                  @change="selectedUid = p.uid"
                />
              </td>
              <td class="col-name">
                <span class="proj-name">{{ p.name }}</span>
                <span v-if="projectMeta.current?.uid === p.uid" class="open-badge"
                  v-tooltip.right="'Currently open project.'">open</span>
              </td>
              <td class="col-type">
                <span class="type-badge" :style="{ color: typeColour[p.type] }">
                  {{ p.type }}
                </span>
              </td>
              <td class="col-path dim" v-tooltip.bottom="p.path">
                {{ p.path.length > 40 ? '…' + p.path.slice(-38) : p.path }}
              </td>
              <td class="col-date dim">{{ formatDate(p.lastOpenedAt) }}</td>
            </tr>
          </tbody>
        </table>
      </div>

      <!-- ── NEW PROJECT tab ───────────────────────────────────────────── -->
      <div v-if="tab === 'new'" class="pp-body pp-form">

        <div class="form-row">
          <label class="form-label"
            v-tooltip.right="'A short, descriptive name for this project. It does not have to match the folder name.'">
            Project name
          </label>
          <input
            class="form-input"
            :class="{ 'input-error': nameError }"
            v-model="newName"
            placeholder="e.g. Tumour microenvironment 2025"
            @keyup.enter="createProject"
            v-tooltip.right="'Give the project a unique, descriptive name.'"
          />
          <span class="field-error" v-if="nameError">{{ nameError }}</span>
        </div>

        <div class="form-row">
          <label class="form-label"
            v-tooltip.right="'Static: fixed-tissue microscopy. Live: time-lapse with cell tracking. Flow: flow/mass cytometry without images.'">
            Project type
          </label>
          <div class="type-options">
            <label
              v-for="opt in typeOptions"
              :key="opt.value"
              class="type-option"
              :class="{ chosen: newType === opt.value }"
              v-tooltip.bottom="opt.tip"
            >
              <input type="radio" :value="opt.value" v-model="newType" />
              {{ opt.label }}
            </label>
          </div>
        </div>

        <div class="form-row">
          <span class="field-hint"
            v-tooltip.right="'Override with the CECELIA_PROJECTS_DIR environment variable when starting the Julia server.'">
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
        <button class="btn-ghost btn-sm" @click="$emit('close')"
          v-tooltip.top="'Close without changes.'">
          Cancel
        </button>

        <template v-if="tab === 'recent'">
          <button
            class="btn-primary btn-sm"
            :disabled="!selectedUid || projectMeta.loading || selectedUid === projectMeta.current?.uid"
            @click="openSelected"
            v-tooltip.top="selectedUid && selectedUid !== projectMeta.current?.uid
              ? 'Open the selected project.'
              : 'Select a project from the list above.'">
            <i class="pi pi-folder-open" />
            Open project
          </button>
        </template>

        <template v-if="tab === 'new'">
          <button
            class="btn-primary btn-sm"
            :disabled="projectMeta.loading"
            @click="createProject"
            v-tooltip.top="'Create the project and open it.'">
            <i class="pi pi-plus" v-if="!projectMeta.loading" />
            <i class="pi pi-spin pi-cog" v-else />
            Create project
          </button>
        </template>
      </div>

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
  font-size: 0.8rem; font-weight: 500;
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

.pp-empty {
  display: flex; flex-direction: column; align-items: center;
  justify-content: center; gap: 0.75rem;
  padding: 3rem 1rem;
  color: var(--cc-text-dim); font-size: 0.85rem;
}
.pp-empty p { margin: 0; }

/* project table */
.proj-table { width: 100%; border-collapse: collapse; font-size: 0.82rem; }
.proj-table thead th {
  text-align: left; font-size: 0.68rem; font-weight: 600;
  text-transform: uppercase; letter-spacing: 0.06em;
  color: var(--cc-text-dim);
  padding: 0.4rem 0.75rem;
  border-bottom: 1px solid var(--cc-border);
  position: sticky; top: 0; background: var(--cc-surface-1);
}
.col-sel  { width: 32px; }
.col-name { min-width: 150px; }
.col-type { width: 80px; }
.col-path { flex: 1; }
.col-date { width: 120px; }

.proj-row {
  border-bottom: 1px solid var(--cc-border);
  cursor: pointer;
  transition: background 0.1s;
}
.proj-row td { padding: 0.45rem 0.75rem; vertical-align: middle; }
.proj-row:hover { background: var(--cc-surface-2); }
.proj-row.selected { background: #a78bfa14; }
.proj-row.active { background: #a78bfa0a; }

.proj-name { color: var(--cc-text); font-weight: 500; margin-right: 0.4rem; }
.open-badge {
  font-size: 0.62rem; font-weight: 700; text-transform: uppercase;
  padding: 0.05rem 0.35rem; border-radius: 0.2rem;
  background: #a78bfa22; color: var(--cc-accent);
  border: 1px solid #a78bfa44;
}
.type-badge { font-size: 0.75rem; font-weight: 600; text-transform: uppercase; }
.dim { color: var(--cc-text-dim); font-size: 0.78rem; }

/* form */
.pp-form { padding: 1.25rem 1.5rem; display: flex; flex-direction: column; gap: 1.25rem; }

.form-row { display: flex; flex-direction: column; gap: 0.35rem; }
.form-label {
  font-size: 0.78rem; font-weight: 600;
  color: var(--cc-text); cursor: default;
}
/* visual styling from the global form base (style.css) */
.form-input.input-error { border-color: #ef4444; }
.form-input::placeholder { color: var(--cc-text-dim); }

.field-error { font-size: 0.75rem; color: #fca5a5; }
.field-hint {
  font-size: 0.72rem; color: var(--cc-text-dim);
  display: flex; align-items: center; gap: 0.3rem;
}
.dir-hint {
  font-family: var(--cc-mono);
  font-size: 0.7rem;
  background: var(--cc-surface-2);
  padding: 0.1rem 0.35rem;
  border-radius: 0.2rem;
  color: var(--cc-text);
}

/* type radio options */
.type-options { display: flex; gap: 0.5rem; }
.type-option {
  display: flex; align-items: center; gap: 0.35rem;
  font-size: 0.8rem; font-weight: 500;
  padding: 0.3rem 0.75rem;
  border-radius: 0.35rem;
  border: 1px solid var(--cc-border);
  background: var(--cc-surface-2);
  color: var(--cc-text-dim);
  cursor: pointer;
  transition: background 0.1s, color 0.1s, border-color 0.1s;
}
.type-option input { display: none; }
.type-option:hover { background: var(--cc-surface-2); color: var(--cc-text); }
.type-option.chosen { border-color: var(--cc-accent); color: var(--cc-accent); background: #a78bfa14; }

/* footer */
.pp-footer {
  display: flex; align-items: center; justify-content: flex-end; gap: 0.4rem;
  padding: 0.65rem 1rem;
  border-top: 1px solid var(--cc-border);
  background: var(--cc-surface-1);
  flex-shrink: 0;
}

.btn-sm {
  display: flex; align-items: center; gap: 0.3rem;
  font-size: 0.78rem; font-weight: 500;
  padding: 0.35rem 0.75rem;
  border-radius: 0.35rem; border: 1px solid transparent;
  cursor: pointer;
}
.btn-ghost { background: var(--cc-surface-2); border-color: var(--cc-border); color: var(--cc-text-dim); }
.btn-ghost:hover { color: var(--cc-text); }
.btn-primary { background: var(--cc-accent); color: #fff; }
.btn-primary:hover:not(:disabled) { filter: brightness(1.1); }
.btn-primary:disabled { opacity: 0.35; cursor: not-allowed; background: var(--cc-surface-2); color: var(--cc-text-dim); border-color: transparent; }
</style>
