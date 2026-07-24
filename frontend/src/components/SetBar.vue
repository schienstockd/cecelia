<script setup lang="ts">
import { ref, computed } from 'vue'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useLogStore } from '../stores/log'

const props = defineProps<{ allowManage?: boolean }>()

const project     = useProjectStore()
const projectMeta = useProjectMetaStore()
const log         = useLogStore()

const newSetName    = ref('')
const showNewInput  = ref(false)
const confirmDelete = ref(false)
const copiedSetUid  = ref(false)

async function copySetUid(uid: string) {
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
  copiedSetUid.value = true
  setTimeout(() => { copiedSetUid.value = false }, 1200)
}
const creating      = ref(false)

const activeSet = computed(() => project.activeSet())

async function createSet() {
  const name = newSetName.value.trim()
  if (!name) { log.warn('Set name cannot be empty.', { source: 'import' }); return }
  if (project.sets.some(s => s.name === name)) {
    log.warn(`A set named "${name}" already exists.`, { source: 'import' }); return
  }
  if (!projectMeta.current) {
    log.warn('No project open.', { source: 'import' }); return
  }
  creating.value = true
  try {
    const res = await fetch('/api/sets/create', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: projectMeta.current.uid, name }),
    })
    const body = await res.json().catch(() => ({})) as { uid?: string; error?: string }
    if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
    project.addSetFromApi(body.uid!, name)
    log.info(`Created set "${name}".`, { source: 'import' })
    newSetName.value = ''
    showNewInput.value = false
  } catch (e) {
    log.error(`Failed to create set: ${e instanceof Error ? e.message : String(e)}`, { source: 'import' })
  } finally {
    creating.value = false
  }
}

async function deleteSet() {
  if (!activeSet.value) return
  const setUid  = activeSet.value.uid
  const setName = activeSet.value.name
  confirmDelete.value = false
  const projectUid = projectMeta.current?.uid
  if (projectUid) {
    try {
      const res = await fetch('/api/sets/delete', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ projectUid, setUid }),
      })
      const body = await res.json().catch(() => ({})) as { error?: string }
      if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
    } catch (e) {
      log.error(`Failed to delete set: ${e instanceof Error ? e.message : String(e)}`, { source: 'import' })
      return
    }
  }
  project.deleteSet(setUid)
  log.info(`Deleted set "${setName}".`, { source: 'import' })
}
</script>

<template>
  <div class="set-bar">
    <div class="set-selector">
      <label class="set-label"
        v-tooltip.bottom="'The active image set. All operations apply to images in this set.'">
        Set
      </label>
      <select
        class="set-select"
        :value="project.activeSetUid ?? ''"
        @change="project.activeSetUid = ($event.target as HTMLSelectElement).value || null"
        v-tooltip.bottom="'Switch between image sets in this project.'"
        :disabled="project.sets.length === 0"
      >
        <option value="" disabled>— no sets —</option>
        <option v-for="s in project.sets" :key="s.uid" :value="s.uid">{{ s.name }}</option>
      </select>
      <template v-if="activeSet">
        <span class="set-uid">{{ activeSet.uid }}</span>
        <button class="set-uid-copy" @click="copySetUid(activeSet.uid)"
          v-tooltip.bottom="copiedSetUid ? 'Copied!' : 'Copy set UID to clipboard'">
          <i :class="copiedSetUid ? 'pi pi-check' : 'pi pi-copy'" />
        </button>
      </template>
    </div>

    <template v-if="allowManage">
      <template v-if="showNewInput">
        <input
          class="set-name-input"
          v-model="newSetName"
          placeholder="Set name…"
          @keydown.enter="createSet"
          @keydown.escape="showNewInput = false"
          autofocus
          v-tooltip.bottom="'Press Enter to create, Escape to cancel.'"
        />
        <button class="cc-btn cc-btn-primary" @click="createSet" :disabled="creating"
          v-tooltip.bottom="'Create this image set.'">
          <i v-if="creating" class="pi pi-spin pi-cog" />
          <template v-else>Create</template>
        </button>
        <button class="cc-btn cc-btn-ghost" @click="showNewInput = false"
          v-tooltip.bottom="'Cancel.'">Cancel</button>
      </template>
      <template v-else>
        <button class="cc-btn cc-btn-ghost" @click="showNewInput = true"
          v-tooltip.bottom="'Create a new image set to group related images together.'">
          <i class="pi pi-plus" /> New set
        </button>
      </template>

      <span class="spacer" />

      <template v-if="activeSet && !confirmDelete">
        <button class="cc-btn cc-btn-danger-ghost" @click="confirmDelete = true"
          v-tooltip.bottom="`Delete set '${activeSet.name}' and all its images. This cannot be undone.`">
          <i class="pi pi-trash" /> Delete set
        </button>
      </template>
      <template v-if="confirmDelete">
        <span class="confirm-text">Delete <strong>{{ activeSet?.name }}</strong>?</span>
        <button class="cc-btn cc-btn-danger-ghost" @click="deleteSet"
          v-tooltip.bottom="'Permanently delete this set and remove all its images from disk.'">
          Confirm
        </button>
        <button class="cc-btn cc-btn-ghost" @click="confirmDelete = false"
          v-tooltip.bottom="'Cancel deletion.'">Cancel</button>
      </template>
    </template>

    <span v-else class="spacer" />
  </div>
</template>

<style scoped>
.set-bar {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.6rem 1rem;
  background: var(--cc-surface-1);
  border-bottom: 1px solid var(--cc-border);
  flex-shrink: 0;
}
.set-selector { display: flex; align-items: center; gap: 0.5rem; }
.set-uid {
  font-family: var(--cc-mono);
  font-size: 0.7rem;
  color: var(--cc-text-dim);
  letter-spacing: 0.03em;
}
.set-uid-copy {
  background: none;
  border: none;
  cursor: pointer;
  color: var(--cc-text-dim);
  font-size: 0.78rem;
  padding: 0.2rem 0.35rem;
  border-radius: 0.25rem;
  line-height: 1;
}
.set-uid-copy:hover { color: var(--cc-text); background: var(--cc-surface-2); }
.set-label {
  font-size: 0.75rem; font-weight: 600; color: var(--cc-text-dim);
  text-transform: uppercase; letter-spacing: 0.06em; cursor: default;
}
/* visual styling from the global form base (style.css) */
.set-select { min-width: 180px; }
.set-select:disabled { opacity: 0.4; cursor: not-allowed; }
.set-name-input { width: 180px; border-color: var(--cc-accent); }
.spacer { flex: 1; }
.confirm-text { font-size: 0.82rem; color: var(--cc-text-dim); }
/* buttons use the global .cc-btn utilities (style.css) */
</style>
