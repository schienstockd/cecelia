<script setup lang="ts">
import { ref, watch, onMounted } from 'vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'

const projectMeta = useProjectMetaStore()
const settings    = useSettingsStore()

const editName = ref(projectMeta.current?.name ?? '')
const saving   = ref(false)
const saved    = ref(false)

// Reset when project changes
watch(() => projectMeta.current?.name, n => { editName.value = n ?? '' })

async function saveName() {
  if (!editName.value.trim() || editName.value === projectMeta.current?.name) return
  saving.value = true
  const ok = await projectMeta.renameProject(editName.value)
  saving.value = false
  if (ok) {
    saved.value = true
    setTimeout(() => { saved.value = false }, 2000)
  }
}

function copyUid() {
  if (projectMeta.current?.uid)
    navigator.clipboard.writeText(projectMeta.current.uid)
}

// ── Software updates ───────────────────────────────────────────────────────
const verCurrent = ref('')
const verLatest = ref<string | null>(null)
const updateAvailable = ref(false)
const checking = ref(false)
const updateBusy = ref(false)
const updateMsg = ref('')

async function checkUpdates() {
  checking.value = true
  updateMsg.value = ''
  try {
    const d = await (await fetch('/api/update/check')).json()
    verCurrent.value = d.current ?? ''
    verLatest.value = d.latest ?? null
    updateAvailable.value = !!d.updateAvailable
    if (d.error) updateMsg.value = d.error
  } catch {
    updateMsg.value = 'Could not reach the update server.'
  } finally {
    checking.value = false
  }
}

async function applyUpdate() {
  if (!verLatest.value || updateBusy.value) return
  updateBusy.value = true
  updateMsg.value = ''
  try {
    const res = await fetch('/api/update/apply', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ version: verLatest.value }),
    })
    const d = await res.json()
    updateMsg.value = res.ok
      ? (d.message ?? `Update ${verLatest.value} staged — restart Cecelia to finish.`)
      : (d.error ?? 'Update failed.')
    if (res.ok) updateAvailable.value = false
  } catch {
    updateMsg.value = 'Update failed (could not reach the server).'
  } finally {
    updateBusy.value = false
  }
}

onMounted(checkUpdates)
</script>

<template>
  <div class="settings-page">
    <h1 class="page-title">Settings</h1>

    <!-- ── Project ─────────────────────────────────────────────────────── -->
    <section class="settings-section">
      <h2 class="section-title">Project</h2>

      <template v-if="projectMeta.current">
        <div class="field">
          <label class="field-label">Name</label>
          <div class="field-row">
            <input
              class="field-input"
              v-model="editName"
              @keydown.enter="saveName"
              placeholder="Project name"
            />
            <button
              class="save-btn"
              :disabled="saving || !editName.trim() || editName === projectMeta.current?.name"
              @click="saveName"
              v-tooltip.right="'Apply the new project name'"
            >
              <i :class="['pi', saved ? 'pi-check' : saving ? 'pi-spin pi-cog' : 'pi-check']" />
              {{ saved ? 'Applied' : 'Apply' }}
            </button>
          </div>
        </div>

        <div class="field">
          <label class="field-label">Project ID</label>
          <div class="field-row">
            <input class="field-input mono" :value="projectMeta.current.uid" readonly />
            <button class="icon-btn" @click="copyUid" v-tooltip.right="'Copy project ID'">
              <i class="pi pi-copy" />
            </button>
          </div>
          <span class="field-hint">Read-only unique identifier used internally.</span>
        </div>
      </template>

      <p v-else class="no-project">No project open. Open or create a project first.</p>
    </section>

    <!-- ── Interface ───────────────────────────────────────────────────── -->
    <section class="settings-section">
      <h2 class="section-title">Interface</h2>

      <div class="field">
        <label class="toggle-row" v-tooltip.right="'When a task starts running, automatically select it in the task manager log panel.'">
          <input type="checkbox" v-model="settings.taskListAutoFollow" />
          <span class="toggle-label">Auto-follow running tasks in task manager</span>
        </label>
      </div>
    </section>

    <!-- ── Software updates ────────────────────────────────────────────── -->
    <section class="settings-section">
      <h2 class="section-title">Software updates</h2>

      <div class="field">
        <label class="field-label">Version</label>
        <div class="field-row">
          <input class="field-input mono" :value="verCurrent || '—'" readonly />
          <button
            class="save-btn"
            :disabled="checking"
            @click="checkUpdates"
            v-tooltip.right="'Check GitHub for a newer release'"
          >
            <i :class="['pi', checking ? 'pi-spin pi-cog' : 'pi-refresh']" />
            {{ checking ? 'Checking…' : 'Check' }}
          </button>
        </div>
        <span v-if="!updateAvailable && verCurrent && !updateMsg" class="field-hint">
          You're on the latest version.
        </span>
      </div>

      <div v-if="updateAvailable" class="field">
        <button
          class="save-btn"
          :disabled="updateBusy"
          @click="applyUpdate"
          v-tooltip.right="`Download ${verLatest} and stage it; restart Cecelia to finish.`"
        >
          <i :class="['pi', updateBusy ? 'pi-spin pi-cog' : 'pi-download']" />
          {{ updateBusy ? 'Updating…' : `Update to ${verLatest}` }}
        </button>
      </div>

      <span v-if="updateMsg" class="field-hint">{{ updateMsg }}</span>
    </section>
  </div>
</template>

<style scoped>
.settings-page {
  max-width: 560px;
  padding: 2rem 2.5rem;
}

.page-title {
  font-size: 1.1rem;
  font-weight: 700;
  color: var(--cc-text);
  margin: 0 0 1.75rem;
}

.settings-section {
  margin-bottom: 2rem;
}

.section-title {
  font-size: 0.72rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  color: var(--cc-text-dim);
  margin: 0 0 0.85rem;
  padding-bottom: 0.4rem;
  border-bottom: 1px solid var(--cc-border);
}

.field {
  margin-bottom: 1.1rem;
}

.field-label {
  display: block;
  font-size: 0.76rem;
  font-weight: 600;
  color: var(--cc-text);
  margin-bottom: 0.3rem;
}

.field-row {
  display: flex;
  gap: 0.5rem;
  align-items: center;
}

/* visual styling from the global form base (style.css) */
.field-input { flex: 1; }
.field-input[readonly] { color: var(--cc-text-dim); cursor: default; }
.field-input.mono { font-family: var(--cc-mono); font-size: 0.74rem; }

.field-hint {
  display: block;
  font-size: 0.7rem;
  color: var(--cc-text-dim);
  margin-top: 0.25rem;
}

.save-btn {
  display: flex;
  align-items: center;
  gap: 0.3rem;
  font-size: 0.76rem;
  padding: 0.35rem 0.7rem;
  border-radius: 0.35rem;
  border: 1px solid var(--cc-accent);
  background: var(--cc-accent);
  color: #fff;
  cursor: pointer;
  transition: opacity 0.12s;
  flex-shrink: 0;
}
.save-btn:disabled { opacity: 0.4; cursor: not-allowed; }
.save-btn:not(:disabled):hover { opacity: 0.85; }

.icon-btn {
  background: none;
  border: none;
  cursor: pointer;
  color: var(--cc-text-dim);
  font-size: 0.82rem;
  padding: 0.3rem 0.4rem;
  border-radius: 0.25rem;
  flex-shrink: 0;
}
.icon-btn:hover { background: var(--cc-surface-2); color: var(--cc-text); }

.toggle-row {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  font-size: 0.82rem;
  color: var(--cc-text);
  cursor: pointer;
  user-select: none;
}
.toggle-row input { accent-color: var(--cc-accent); cursor: pointer; }

.no-project {
  font-size: 0.8rem;
  color: var(--cc-text-dim);
}
</style>
