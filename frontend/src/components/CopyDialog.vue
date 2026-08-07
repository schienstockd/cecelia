<!--
  CopyDialog — "Copy images", opened from the Copy action in the Import page's action bar. Duplicates
  one version of each selected image into a NEW image (fresh uid) in a new or existing set, dropping all
  derived data — a re-import shortcut so a pipeline can be re-run from a clean copy without re-importing
  the microscope file. Wraps a version picker (like CropDialog) + the shared destination-set picker in a
  modal, then dispatches one editImages.copyImage per image over the task rail (background + universal
  toast + task console) and closes immediately — the copies appear in the target set as the tasks finish.
-->
<script setup lang="ts">
import { ref, computed } from 'vue'
import BaseModal from './BaseModal.vue'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useTaskStore } from '../stores/tasks'
import { useWsStore } from '../stores/ws'
import { useLogStore } from '../stores/log'
import { resolveSetDestination, destinationParams } from '../utils/setDestination'
import type { CciaImage } from '../stores/project'

const props = defineProps<{ images: CciaImage[]; setUid: string }>()
const emit  = defineEmits<{ (e: 'close'): void }>()

const project     = useProjectStore()
const projectMeta = useProjectMetaStore()
const taskStore   = useTaskStore()
const ws          = useWsStore()
const log         = useLogStore()

const single = computed(() => props.images.length === 1 ? props.images[0] : null)

// Every registered version/iteration is a valid copy source. With several images selected only the
// versions they ALL carry can be copied in one go, so the picker offers the intersection.
const valueNames = computed(() => {
  const lists = props.images.map(i => Object.keys(i.filepaths ?? {}))
  if (!lists.length) return []
  return lists.reduce((common, names) => common.filter(n => names.includes(n)))
})
const selectedValueName = ref(defaultValueName())
function defaultValueName(): string {
  const names = valueNames.value
  const img = single.value
  // One image: default to the active version (mirrors CropDialog). Several: the imported original,
  // since "the active version" is per image and would mean something different for each.
  if (!img) return names.includes('default') ? 'default' : (names[0] ?? '')
  const nonDefault = names.filter(n => n !== 'default')
  return img.activeValueName && names.includes(img.activeValueName) ? img.activeValueName
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

function copyImages() {
  if (copying.value || !props.images.length) return
  const dest = resolveSetDestination(project.sets, targetUid.value, newName.value)
  if (!dest.ok) { log.warn(dest.error, { source: 'copy' }); return }
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  const params = { valueName: selectedValueName.value || 'default', ...destinationParams(dest) }
  copying.value = true
  // One task per image — the copy task is per-image, and the rail shows N of them. addMany (not N ×
  // add) so the batch raises ONE "running in the background" toast instead of one per image. The FIRST
  // task creates the set when in new-set mode; the rest resolve the same name to it server-side.
  const label = props.images.length > 1 ? `Copy ${props.images.length} images` : 'Copy image'
  const entries = taskStore.addMany(props.images.map(img => ({
    module: 'copy', label: 'Copy image', imageUid: img.uid,
    imageName: img.name || img.uid,
    status: 'queued' as const, taskName: 'copyImage', funName: 'editImages.copyImage', params, projectUid,
  })), label)
  entries.forEach((task, i) => {
    ws.send({
      type: 'task:run', taskId: task.id, funName: 'editImages.copyImage', params,
      imageUid: props.images[i].uid, projectUid, setUid: props.setUid, poolName: 'io',
    })
  })
  log.info(`Copying ${props.images.length} image(s) — they appear when the tasks finish.`, { source: 'copy' })
  emit('close')   // runs in the background — don't sit on the dialog
}
</script>

<template>
  <BaseModal width="460px" @close="$emit('close')">
    <template #title>
      <i class="pi pi-copy" />
      Copy — {{ single ? single.name : `${images.length} images` }}
      <span v-if="selectedValueName" class="copy-version-tag">{{ selectedValueName }}</span>
    </template>

    <div v-if="valueNames.length > 1" class="copy-row">
      <span class="copy-lbl cc-muted" v-tooltip.right="'Which image version to copy (becomes the copy\'s default)'">Version</span>
      <select v-model="selectedValueName" class="copy-select" v-tooltip.right="'Which image version to copy'">
        <option v-for="vn in valueNames" :key="vn" :value="vn">{{ vn }}</option>
      </select>
    </div>

    <div class="copy-row">
      <span class="copy-lbl cc-muted" v-tooltip.right="'Where to put the copies (data IS duplicated on disk)'">To set</span>
      <select v-model="targetUid" class="copy-select" v-tooltip.right="'Set the copies are placed in'">
        <option v-for="s in allSets" :key="s.uid" :value="s.uid">{{ s.name }}</option>
        <option value="">＋ New set…</option>
      </select>
    </div>
    <div v-if="!targetUid" class="copy-row">
      <span class="copy-lbl cc-muted" />
      <input class="copy-name-input" v-model="newName" placeholder="New set name…"
             v-tooltip.right="'Name for the new set'" @keydown.enter="copyImages" autofocus />
    </div>

    <p class="copy-hint cc-muted cc-fs-xs">
      Copies the version as a new image. Segmentations, populations and gating are dropped.
    </p>

    <template #footer>
      <button class="cc-btn cc-btn-ghost" @click="$emit('close')">Cancel</button>
      <button class="cc-btn cc-btn-primary" :disabled="copying" @click="copyImages">
        <i class="pi pi-copy" /> Copy {{ images.length > 1 ? `${images.length} images` : 'image' }}
      </button>
    </template>
  </BaseModal>
</template>

<style scoped>
.copy-row { display: flex; align-items: center; gap: 0.6rem; margin-bottom: 0.55rem; }
.copy-lbl { width: 4rem; flex-shrink: 0; }
.copy-select, .copy-name-input { flex: 1 1 auto; }
.copy-hint { font-style: italic; margin: 0.3rem 0 0; }   /* + .cc-muted .cc-fs-xs */
.copy-version-tag {
  margin-left: 0.4rem; padding: 0.05rem 0.4rem; border-radius: var(--cc-radius-lg);
  font-size: var(--cc-fs-2xs); font-weight: 600; vertical-align: middle;
  color: var(--cc-accent);
  background: color-mix(in srgb, var(--cc-accent) 15%, transparent);
}
</style>
