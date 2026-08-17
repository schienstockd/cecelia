<script setup lang="ts">
import { ref, computed } from 'vue'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useLogStore } from '../stores/log'
import { useCopyFlash } from '../composables/useCopyFlash'
import { setNameTaken } from '../utils/setDestination'

const props = defineProps<{ allowManage?: boolean }>()

const project     = useProjectStore()
const projectMeta = useProjectMetaStore()
const log         = useLogStore()

const newSetName    = ref('')
const showNewInput  = ref(false)
const confirmDelete = ref(false)
// Copy set UID — shared copy+flash helper (docs/UI.md → UX-primitive catalog)
const { isCopied: copiedSetUid, copy: copySetUid } = useCopyFlash()
const busy          = ref(false)

const activeSet = computed(() => project.activeSet())

// ONE name input serving create and rename, the same state machine ChainModule uses for its
// New/Rename pair — a second input would be a second thing to keep in sync and would double the width
// of a bar that already holds the picker, the uid and three buttons.
const nameMode = ref<'create' | 'rename'>('create')

function openNameInput(mode: 'create' | 'rename') {
  // clicking the button that is already open closes it, so the button is its own cancel
  if (showNewInput.value && nameMode.value === mode) { closeNameInput(); return }
  nameMode.value = mode
  // Rename PREFILLS: a rename is usually an edit ("day3" → "day 3"), so retyping the whole name is
  // work the user has already done once.
  newSetName.value = mode === 'rename' ? (activeSet.value?.name ?? '') : ''
  showNewInput.value = true
}

function closeNameInput() {
  showNewInput.value = false
  newSetName.value = ''
}

const submitName = () => (nameMode.value === 'rename' ? renameSet() : createSet())

async function createSet() {
  const name = newSetName.value.trim()
  if (!name) { log.warn('Set name cannot be empty.', { source: 'manageImages' }); return }
  if (setNameTaken(project.sets, name)) {
    log.warn(`A set named "${name}" already exists.`, { source: 'manageImages' }); return
  }
  if (!projectMeta.current) {
    log.warn('No project open.', { source: 'manageImages' }); return
  }
  busy.value = true
  try {
    const res = await fetch('/api/sets/create', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: projectMeta.current.uid, name }),
    })
    const body = await res.json().catch(() => ({})) as { uid?: string; error?: string }
    if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
    project.addSetFromApi(body.uid!, name)
    log.info(`Created set "${name}".`, { source: 'manageImages' })
    closeNameInput()
  } catch (e) {
    log.error(`Failed to create set: ${e instanceof Error ? e.message : String(e)}`, { source: 'manageImages' })
  } finally {
    busy.value = false
  }
}

// Rename the active set. The name is display-only (the uid is the identity), so nothing moves on disk
// and nothing else in the store has to be told — see `rename_set!` in app/src/model/project.jl.
async function renameSet() {
  const set  = activeSet.value
  const name = newSetName.value.trim()
  if (!set) return
  if (!name) { log.warn('Set name cannot be empty.', { source: 'manageImages' }); return }
  if (name === set.name) { closeNameInput(); return }      // no-op, not an error
  // Pre-flight only — the real guard is `set_name_taken` in the model, which answers this for the REPL
  // too and makes the route 409. Kept here so the common case is an instant warning instead of a round
  // trip. Excludes THIS set, so re-submitting its own name is fine.
  if (setNameTaken(project.sets, name, set.uid)) {
    log.warn(`A set named "${name}" already exists.`, { source: 'manageImages' }); return
  }
  if (!projectMeta.current) {
    log.warn('No project open.', { source: 'manageImages' }); return
  }
  busy.value = true
  try {
    const res = await fetch('/api/sets/rename', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: projectMeta.current.uid, setUid: set.uid, name }),
    })
    const body = await res.json().catch(() => ({})) as { error?: string }
    if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
    project.renameSet(set.uid, name)
    log.info(`Renamed set to "${name}".`, { source: 'manageImages' })
    closeNameInput()
  } catch (e) {
    log.error(`Failed to rename set: ${e instanceof Error ? e.message : String(e)}`, { source: 'manageImages' })
  } finally {
    busy.value = false
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
      log.error(`Failed to delete set: ${e instanceof Error ? e.message : String(e)}`, { source: 'manageImages' })
      return
    }
  }
  project.deleteSet(setUid)
  log.info(`Deleted set "${setName}".`, { source: 'manageImages' })
}
</script>

<template>
  <div class="set-bar">
    <div class="set-selector">
      <label class="set-label"
        v-tooltip.bottom="'The active image set — all operations apply to it'">
        Set
      </label>
      <select
        class="set-select"
        data-guide="set.select"
        :value="project.activeSetUid ?? ''"
        @change="project.activeSetUid = ($event.target as HTMLSelectElement).value || null"
        v-tooltip.bottom="'Switch between image sets in this project'"
        :disabled="project.sets.length === 0"
      >
        <option value="" disabled>— no sets —</option>
        <option v-for="s in project.sets" :key="s.uid" :value="s.uid">{{ s.name }}</option>
      </select>
      <template v-if="activeSet">
        <span class="set-uid cc-muted cc-fs-xs">{{ activeSet.uid }}</span>
        <button class="set-uid-copy cc-btn cc-btn-bare cc-btn-icon" @click="copySetUid(activeSet.uid)"
          v-tooltip.bottom="copiedSetUid() ? 'Copied!' : 'Copy set UID to clipboard'">
          <i :class="copiedSetUid() ? 'pi pi-check' : 'pi pi-copy'" />
        </button>
      </template>
    </div>

    <template v-if="allowManage">
      <template v-if="showNewInput">
        <input
          class="set-name-input"
          v-model="newSetName"
          :placeholder="nameMode === 'rename' ? 'New name…' : 'Set name…'"
          @keydown.enter="submitName"
          @keydown.escape="closeNameInput"
          autofocus
          v-tooltip.bottom="nameMode === 'rename' ? 'Press Enter to rename, Escape to cancel'
                                                  : 'Press Enter to create, Escape to cancel'"
        />
        <button class="cc-btn cc-btn-primary" @click="submitName" :disabled="busy"
          v-tooltip.bottom="nameMode === 'rename' ? 'Rename this image set' : 'Create this image set'">
          <i v-if="busy" class="pi pi-spin pi-spinner" />
          <template v-else>{{ nameMode === 'rename' ? 'Rename' : 'Create' }}</template>
        </button>
        <button class="cc-btn cc-btn-ghost" @click="closeNameInput"
          v-tooltip.bottom="'Cancel'">Cancel</button>
      </template>
      <template v-else>
        <button class="cc-btn cc-btn-ghost" data-guide="set.new" @click="openNameInput('create')"
          v-tooltip.bottom="'Create a new image set to group related images together'">
          <i class="pi pi-plus" /> New set
        </button>
        <button v-if="activeSet" class="cc-btn cc-btn-ghost" @click="openNameInput('rename')"
          v-tooltip.bottom="`Rename set '${activeSet.name}' — the images and their data are untouched`">
          <i class="pi pi-pencil" /> Rename
        </button>
      </template>

      <span class="spacer" />

      <template v-if="activeSet && !confirmDelete">
        <button class="cc-btn cc-btn-danger-ghost" @click="confirmDelete = true"
          v-tooltip.bottom="`Delete set '${activeSet.name}' and all its images — cannot be undone`">
          <i class="pi pi-trash" /> Delete set
        </button>
      </template>
      <template v-if="confirmDelete">
        <span class="confirm-text cc-muted cc-fs-md">Delete <strong>{{ activeSet?.name }}</strong>?</span>
        <button class="cc-btn cc-btn-danger-ghost" @click="deleteSet"
          v-tooltip.bottom="'Permanently delete this set and remove all its images from disk'">
          Confirm
        </button>
        <button class="cc-btn cc-btn-ghost" @click="confirmDelete = false"
          v-tooltip.bottom="'Cancel deletion'">Cancel</button>
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
.set-uid { font-family: var(--cc-mono); letter-spacing: 0.03em; }
/* .set-uid-copy → cc-btn cc-btn-bare cc-btn-icon */
.set-uid-copy:hover { color: var(--cc-text); background: var(--cc-surface-2); }
.set-label {
  font-size: var(--cc-fs-sm); font-weight: 600; color: var(--cc-text-dim);
  text-transform: uppercase; letter-spacing: 0.06em; cursor: default;
}
/* visual styling from the global form base (style.css) */
.set-select { min-width: 180px; }
.set-select:disabled { opacity: 0.4; cursor: not-allowed; }
.set-name-input { width: 180px; border-color: var(--cc-accent); }
.spacer { flex: 1; }

/* buttons use the global .cc-btn utilities (style.css) */
</style>
