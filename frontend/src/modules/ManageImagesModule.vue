<script setup lang="ts">
import { ref, computed } from 'vue'
import { useProjectStore } from '../stores/project'
import { useLogStore } from '../stores/log'
import { useProjectMetaStore } from '../stores/projectMeta'
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import FileBrowser from '../components/FileBrowser.vue'
import ImageFileActions from '../components/ImageFileActions.vue'
import LegacyMigrateDialog from '../components/LegacyMigrateDialog.vue'
import SeriesPickerModal from '../components/SeriesPickerModal.vue'
import { useTaskDefs } from '../composables/useTaskDefs'
import { buildRegisterRecords, isProbeableMultiSeriesPath,
         type RegisterRecord, type SeriesEntry } from '../utils/seriesPicker'

// The page is 'Manage images', not 'Import': it hosts add/copy/move/delete alongside the import
// tasks, and now the export ones too. Two categories rather than one — `exportImages` is its own
// category because an export produces an ARTEFACT outside the project, unlike `editImages` (copy,
// crop), which produces new images inside it. Import first, so the picker reads in workflow order.
const { defs: importDefs, reload: reloadDefs } = useTaskDefs(['importImages', 'exportImages'])
const project     = useProjectStore()
const log         = useLogStore()
const projectMeta = useProjectMetaStore()

const activeSet   = computed(() => project.activeSet())
const showBrowser = ref(false)
const showMigrate = ref(false)

// Series-picker queue: a multi-series file (LIF, …) shows one picker before it lands in the register
// payload; a single-series file joins the payload as-is. `pickerQueue` is the deque of remaining
// probeable paths; `pickerCurrent` is the one whose modal is up (null when none). Kept out of the
// register loop so opening a modal doesn't have to block a promise chain the browser can't unwind.
const pickerQueue      = ref<string[]>([])
const pickerCurrent    = ref<string | null>(null)
const pendingRecords   = ref<RegisterRecord[]>([])
const pendingSetUid    = ref<string | null>(null)

function openFilePicker() {
  if (!activeSet.value) {
    log.warn('Create or select a set before adding images.', { source: 'manageImages' })
    return
  }
  showBrowser.value = true
}

function openMigrate() {
  if (!activeSet.value) {
    log.warn('Create or select a set before migrating a legacy project.', { source: 'manageImages' })
    return
  }
  showMigrate.value = true
}

function onLegacyImported(images: unknown[]) {
  // keep the dialog open — it shows the "now run Migrate legacy image" next-step panel itself
  const set = activeSet.value
  if (!set) return
  project.addImagesFromApi(set.uid, images as never[])
  const n = images.length
  log.info(
    `Added ${n} legacy image${n !== 1 ? 's' : ''} to "${set.name}". ` +
    `Run the "Migrate legacy image" task to transfer the data.`,
    { source: 'manageImages' },
  )
}

async function onFilesSelected(paths: string[]) {
  const set = activeSet.value
  if (!set || !projectMeta.current) return
  showBrowser.value = false

  // Single-series files go straight into the register payload; multi-series-capable files are
  // queued and the modal opens for each in turn (see advancePicker). When the queue drains we POST
  // /api/images/register once, so a mixed selection makes ONE request rather than N.
  pendingRecords.value = []
  pendingSetUid.value  = set.uid
  const queue: string[] = []
  for (const p of paths) {
    isProbeableMultiSeriesPath(p) ? queue.push(p) : pendingRecords.value.push({ path: p })
  }
  pickerQueue.value = queue
  advancePicker()
}

function advancePicker() {
  if (pickerQueue.value.length === 0) {
    pickerCurrent.value = null
    submitRegister()
    return
  }
  pickerCurrent.value = pickerQueue.value[0]
  pickerQueue.value   = pickerQueue.value.slice(1)
}

function onPickerSave(picks: SeriesEntry[]) {
  const path = pickerCurrent.value
  if (path) pendingRecords.value.push(...buildRegisterRecords(path, picks))
  advancePicker()
}

function onPickerCancel() {
  // Cancel = skip THIS file only. The rest of the queue (and the already-collected records) survive.
  advancePicker()
}

async function submitRegister() {
  const setUid = pendingSetUid.value
  const set    = activeSet.value
  if (!setUid || !set || !projectMeta.current) return
  const records = pendingRecords.value
  pendingRecords.value = []
  pendingSetUid.value  = null
  if (records.length === 0) return
  try {
    const res = await fetch('/api/images/register', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        projectUid: projectMeta.current.uid,
        setUid,
        filepaths:  records,
      }),
    })
    const body = await res.json().catch(() => ({})) as { images?: any[]; error?: string }
    if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
    const imgs = body.images ?? []
    project.addImagesFromApi(setUid, imgs)
    log.info(
      `Added ${imgs.length} image${imgs.length !== 1 ? 's' : ''} to "${set.name}".`,
      { source: 'manageImages' }
    )
  } catch (e) {
    log.error(
      `Failed to register images: ${e instanceof Error ? e.message : String(e)}`,
      { source: 'manageImages' }
    )
  }
}
</script>

<template>
  <!-- file browser modal — rendered outside ModuleLayout so it overlays everything -->
  <FileBrowser
    v-if="showBrowser"
    @select="onFilesSelected"
    @close="showBrowser = false"
  />

  <LegacyMigrateDialog
    v-if="showMigrate && activeSet && projectMeta.current"
    :project-uid="projectMeta.current.uid"
    :set-uid="activeSet.uid"
    @imported="onLegacyImported"
    @close="showMigrate = false"
  />

  <SeriesPickerModal
    v-if="pickerCurrent"
    :key="pickerCurrent"
    :filepath="pickerCurrent"
    @save="onPickerSave"
    @cancel="onPickerCancel"
  />

  <ModuleLayout
    module="manageImages"
    :allow-manage="true"
    :show-filter="false"
    no-set-hint="Create a set to get started."
  >
    <template #actions="{ hasSet, setUid, selectedUids, selectUids }">
      <button
        class="cc-btn cc-btn-primary"
        data-guide="manageImages.addImages"
        :disabled="!hasSet"
        @click="openFilePicker"
        v-tooltip.bottom="hasSet
          ? 'Browse and select microscopy image files to add to this set'
          : 'Create or select a set first, then add images'"
      >
        <i class="pi pi-plus" /> Add images
      </button>
      <button
        class="cc-btn"
        :disabled="!hasSet"
        @click="openMigrate"
        v-tooltip.bottom="hasSet
          ? 'Import images, segmentation and tracking from an old (R/Shiny) cecelia project'
          : 'Create or select a set first'"
      >
        <i class="pi pi-history" /> Migrate legacy project
      </button>

      <!-- the standard file operations, applied to the whole checkbox selection. Import ONLY — no other
           module page can delete or re-file an image (see ImageFileActions.vue). -->
      <ImageFileActions v-if="setUid" :set-uid="setUid" :uids="selectedUids"
        @done="selectUids([])" />
    </template>

    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="importDefs"
        :on-reload-defs="reloadDefs"
        module="manageImages"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>
  </ModuleLayout>
</template>
