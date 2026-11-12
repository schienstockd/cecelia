<script setup lang="ts">
import { ref, computed } from 'vue'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useLogStore } from '../stores/log'
import { useCopyFlash } from '../composables/useCopyFlash'
import { setNameTaken } from '../utils/setDestination'
import ConfirmButton from './ConfirmButton.vue'
import SetRecipePicker from './SetRecipePicker.vue'

// `inline`: sits inside a module page's set row (ModuleLayout) — no bar chrome of its own, no ID (the
// row's ⋯ menu copies it) and no trailing spacer, so the row's counts and checks follow it.
const props = defineProps<{ allowManage?: boolean; inline?: boolean }>()

const project     = useProjectStore()
const projectMeta = useProjectMetaStore()
const log         = useLogStore()

const newSetName    = ref('')
const showNewInput  = ref(false)
// Copy set UID — shared copy+flash helper (docs/ui/PRIMITIVES.md)
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
  const projectUid = projectMeta.current?.uid
  if (projectUid) {
    busy.value = true
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
    } finally {
      busy.value = false
    }
  }
  project.deleteSet(setUid)
  log.info(`Deleted set "${setName}".`, { source: 'manageImages' })
}
</script>

<template>
  <div class="set-bar" :class="{ inline }">
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
      <template v-if="activeSet && !inline">
        <span class="set-uid cc-muted cc-fs-xs">{{ activeSet.uid }}</span>
        <button class="set-uid-copy cc-btn cc-btn-bare cc-btn-icon" @click="copySetUid(activeSet.uid)"
          v-tooltip.bottom="copiedSetUid() ? 'Copied!' : 'Copy set UID to clipboard'">
          <i :class="copiedSetUid() ? 'pi pi-check' : 'pi pi-copy'" />
        </button>
      </template>
    </div>

    <SetRecipePicker />

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
        <!-- inline (the module set row): the same buttons, icon-only, so the row keeps its counts -->
        <button class="cc-btn cc-btn-ghost" :class="{ 'cc-btn-icon': inline }" data-guide="set.new"
          @click="openNameInput('create')" aria-label="New set"
          v-tooltip.bottom="'Create a new image set to group related images together'">
          <i class="pi pi-plus" /><template v-if="!inline"> New set</template>
        </button>
        <button v-if="activeSet" class="cc-btn cc-btn-ghost" :class="{ 'cc-btn-icon': inline }"
          @click="openNameInput('rename')" aria-label="Rename set"
          v-tooltip.bottom="`Rename set '${activeSet.name}' — the images and their data are untouched`">
          <i class="pi pi-pencil" /><template v-if="!inline"> Rename</template>
        </button>
      </template>

      <span v-if="!inline" class="spacer" />

      <!-- the set's NAME rides on the armed tooltip: deleting a whole set is the one place a wrong
           target costs every image in it -->
      <ConfirmButton v-if="activeSet" @confirm="deleteSet" v-slot="{ armed, press }">
        <button class="cc-btn" :class="[armed ? 'cc-btn-danger' : 'cc-btn-danger-ghost', { 'cc-btn-icon': inline && !armed }]"
          @click="press" :disabled="busy" aria-label="Delete set"
          v-tooltip.bottom="armed ? `Click again — permanently delete '${activeSet.name}' and its images from disk`
                                  : `Delete set '${activeSet.name}' and all its images — cannot be undone`">
          <i class="pi" :class="busy ? 'pi-spin pi-spinner' : armed ? 'pi-exclamation-triangle' : 'pi-trash'" /><template v-if="!inline || armed"> Delete set</template>
        </button>
      </ConfirmButton>
    </template>

    <span v-else-if="!inline" class="spacer" />
  </div>
</template>

<style scoped>
.set-bar {
  display: flex;
  align-items: center;
  gap: var(--cc-space-sm);
  padding: var(--cc-bar-pad);
  background: var(--cc-surface-1);
  border-bottom: 1px solid var(--cc-border);
  flex-shrink: 0;
}
.set-bar.inline { padding: 0; background: none; border-bottom: none; gap: var(--cc-space-2xs); }
.set-bar.inline .set-select { min-width: 140px; max-width: 14rem; }
.set-selector { display: flex; align-items: center; gap: var(--cc-space-sm); }
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
