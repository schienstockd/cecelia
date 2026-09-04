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
         unsupportedMultiSeriesExts,
         type ProbeResult, type RegisterRecord, type SeriesEntry } from '../utils/seriesPicker'
import { formatSupportRequestUrl } from '../lib/links'

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

// Series-picker queue: a multi-series file (LIF) shows one picker before it lands in the register
// payload; a single-series file joins the payload as-is. `pickerQueue` is the deque of remaining
// probeable paths; `pickerCurrent` is the one whose modal is up (null when none). Kept out of the
// register loop so opening a modal doesn't have to block a promise chain the browser can't unwind.
// The queue carries the pre-fetched ProbeResult with each entry so the modal is a pure view — the
// probe already ran, and we only enqueued files with ≥2 series.
type PickerEntry = { path: string; probe: ProbeResult }
const pickerQueue      = ref<PickerEntry[]>([])
const pickerCurrent    = ref<PickerEntry | null>(null)
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

async function probeSeries(filepath: string): Promise<ProbeResult | null> {
  try {
    const r = await fetch('/api/import/series/probe', {
      method:  'POST',
      headers: { 'Content-Type': 'application/json' },
      body:    JSON.stringify({ filepath }),
    })
    const d = await r.json()
    if (!r.ok) throw new Error(d.error ?? `HTTP ${r.status}`)
    return d as ProbeResult
  } catch (e) {
    // A probe failure isn't fatal — the file still imports as a single-series file (bf2raw picks
    // series 0 by default). We just skip the picker for it and log the reason.
    log.warn(
      `Series probe failed for ${filepath}: ${e instanceof Error ? e.message : String(e)}. ` +
      `Importing as single-series.`,
      { source: 'manageImages' }
    )
    return null
  }
}

async function onFilesSelected(paths: string[]) {
  const set = activeSet.value
  if (!set || !projectMeta.current) return
  showBrowser.value = false

  pendingRecords.value = []
  pendingSetUid.value  = set.uid

  // Probe every probeable file up front, in parallel. Only ≥2 series routes to the modal; a
  // single-series LIF (there's always one snapshot on the same file) never sees the picker. Every
  // other file goes straight into the register payload as before.
  const probePromises: Array<Promise<void>> = []
  const queue: PickerEntry[] = []
  for (const path of paths) {
    if (!isProbeableMultiSeriesPath(path)) {
      pendingRecords.value.push({ path })
      continue
    }
    probePromises.push((async () => {
      const probe = await probeSeries(path)
      if (probe && probe.series.length > 1) {
        queue.push({ path, probe })
      } else {
        // 0 or 1 series → no choice to make, add as single-series (the classic path).
        pendingRecords.value.push({ path })
      }
    })())
  }
  await Promise.all(probePromises)

  // One-shot hint for formats we don't yet probe (CZI/ND2/OIR/IMS/LSM). These silently pick series 0
  // today; a request with a sample file is the fastest way to widen readlif's coverage (see
  // formatSupportRequestUrl → feature_request.yml). One log line per BATCH, not per file.
  const unsupportedExts = unsupportedMultiSeriesExts(paths)
  if (unsupportedExts.length > 0) {
    log.info(
      `Series picking is LIF-only today; ${unsupportedExts.map(e => '.' + e).join(', ')} will ` +
      `import series 0 by default. Request support (a sample file helps): ` +
      formatSupportRequestUrl(unsupportedExts),
      { source: 'manageImages' }
    )
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
  const entry = pickerCurrent.value
  if (entry) pendingRecords.value.push(...buildRegisterRecords(entry.path, picks))
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
    :key="pickerCurrent.path"
    :filepath="pickerCurrent.path"
    :probe="pickerCurrent.probe"
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
