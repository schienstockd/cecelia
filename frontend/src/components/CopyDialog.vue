<!--
  CopyDialog — "Copy image", opened from the Copy action on each ImageTable row. Duplicates ONE image
  version into a NEW image (fresh uid) in a new or existing set, dropping all derived data — a re-import
  shortcut so a pipeline can be re-run from a clean copy without re-importing the microscope file. Wraps
  a version picker (like CropDialog) + a destination-set picker (like ImageTable's move bar) in a modal,
  then dispatches editImages.copyImage over the task rail (background + universal toast + task console)
  and closes immediately — the copy appears in the target set when the task finishes.
-->
<script setup lang="ts">
import { ref, computed } from 'vue'
import BaseModal from './BaseModal.vue'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useTaskStore } from '../stores/tasks'
import { useWsStore } from '../stores/ws'
import { useLogStore } from '../stores/log'
import type { CciaImage } from '../stores/project'

const props = defineProps<{ image: CciaImage; setUid: string }>()
const emit  = defineEmits<{ (e: 'close'): void }>()

const project     = useProjectStore()
const projectMeta = useProjectMetaStore()
const taskStore   = useTaskStore()
const ws          = useWsStore()
const log         = useLogStore()

// Every registered version/iteration of this image is a valid copy source; default to the active one
// (mirrors CropDialog). Empty → the backend falls back to 'default'.
const valueNames = computed(() => Object.keys(props.image.filepaths ?? {}))
const selectedValueName = ref(defaultValueName())
function defaultValueName(): string {
  const names = valueNames.value
  const nonDefault = names.filter(n => n !== 'default')
  return props.image.activeValueName && names.includes(props.image.activeValueName) ? props.image.activeValueName
    : nonDefault.length > 0 ? nonDefault[nonDefault.length - 1]
    : names.includes('default') ? 'default' : (names[0] ?? '')
}

// Destination picker: every set in the project (incl. the current one — a copy into the same set is a
// legit duplicate) plus "＋ New set…". Defaults to new-set mode, since copying into a fresh set is the
// point (re-run a pipeline on a clean copy). '' as target => create a set from newSetName.
const allSets     = computed(() => project.sets)
const targetUid   = ref('')                       // '' = create a new set from newName
const newName     = ref('')
const copying     = ref(false)

function copyImage() {
  if (copying.value) return
  const newSetName = newName.value.trim()
  if (!targetUid.value && !newSetName) {
    log.warn('Pick a set or enter a new set name.', { source: 'copy' }); return
  }
  if (!targetUid.value && project.sets.some(s => s.name === newSetName)) {
    log.warn(`A set named "${newSetName}" already exists.`, { source: 'copy' }); return
  }
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  const params = {
    valueName: selectedValueName.value || 'default',
    ...(targetUid.value ? { toSetUid: targetUid.value } : { newSetName }),
  }
  copying.value = true
  const task = taskStore.add({
    module: 'copy', label: 'Copy image', imageUid: props.image.uid,
    imageName: props.image.name || props.image.uid,
    status: 'queued', taskName: 'copyImage', funName: 'editImages.copyImage', params, projectUid,
  })
  ws.send({
    type: 'task:run', taskId: task.id, funName: 'editImages.copyImage', params,
    imageUid: props.image.uid, projectUid, setUid: props.setUid, poolName: 'io',
  })
  log.info('Copying image → new image in the set (appears when the task finishes).', { source: 'copy' })
  emit('close')   // runs in the background — don't sit on the dialog
}
</script>

<template>
  <BaseModal width="460px" @close="$emit('close')">
    <template #title>
      <i class="pi pi-copy" /> Copy — {{ image.name }}
      <span v-if="selectedValueName" class="copy-version-tag">{{ selectedValueName }}</span>
    </template>

    <div v-if="valueNames.length > 1" class="copy-row">
      <span class="copy-lbl" v-tooltip.right="'Which image version to copy (becomes the copy\'s default)'">Version</span>
      <select v-model="selectedValueName" class="copy-select">
        <option v-for="vn in valueNames" :key="vn" :value="vn">{{ vn }}</option>
      </select>
    </div>

    <div class="copy-row">
      <span class="copy-lbl" v-tooltip.right="'Where to put the copy (data IS duplicated on disk).'">To set</span>
      <select v-model="targetUid" class="copy-select">
        <option v-for="s in allSets" :key="s.uid" :value="s.uid">{{ s.name }}</option>
        <option value="">＋ New set…</option>
      </select>
    </div>
    <div v-if="!targetUid" class="copy-row">
      <span class="copy-lbl" />
      <input class="copy-name-input" v-model="newName" placeholder="New set name…"
             @keydown.enter="copyImage" autofocus />
    </div>

    <p class="copy-hint">
      Duplicates the chosen version as a new image; derived data (segmentations, populations, gating) is
      dropped. Runs in the background — the copy appears when it finishes.
    </p>

    <template #footer>
      <button class="cc-btn cc-btn-ghost" @click="$emit('close')">Cancel</button>
      <button class="cc-btn cc-btn-primary" :disabled="copying" @click="copyImage">
        <i class="pi pi-copy" /> Copy image
      </button>
    </template>
  </BaseModal>
</template>

<style scoped>
.copy-row { display: flex; align-items: center; gap: 0.6rem; margin-bottom: 0.55rem; }
.copy-lbl { color: var(--cc-text-muted, #888); font-size: 0.72rem; width: 4rem; flex-shrink: 0; }
.copy-select, .copy-name-input { flex: 1 1 auto; }
.copy-hint { font-size: 0.68rem; color: var(--cc-text-muted, #888); font-style: italic; margin: 0.3rem 0 0; }
.copy-version-tag {
  margin-left: 0.4rem; padding: 0.05rem 0.4rem; border-radius: 0.5rem;
  font-size: 0.62rem; font-weight: 600; vertical-align: middle;
  color: var(--cc-accent, #a855f7);
  background: color-mix(in srgb, var(--cc-accent, #a855f7) 15%, transparent);
}
</style>
