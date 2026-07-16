<script setup lang="ts">
import { ref, computed } from 'vue'
import { useProjectStore } from '../stores/project'
import { useLogStore } from '../stores/log'
import { useProjectMetaStore } from '../stores/projectMeta'
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import FileBrowser from '../components/FileBrowser.vue'
import LegacyMigrateDialog from '../components/LegacyMigrateDialog.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: importDefs, reload: reloadDefs } = useTaskDefs('importImages')
const project     = useProjectStore()
const log         = useLogStore()
const projectMeta = useProjectMetaStore()

const activeSet   = computed(() => project.activeSet())
const showBrowser = ref(false)
const showMigrate = ref(false)

function openFilePicker() {
  if (!activeSet.value) {
    log.warn('Create or select a set before adding images.', { source: 'import' })
    return
  }
  showBrowser.value = true
}

function openMigrate() {
  if (!activeSet.value) {
    log.warn('Create or select a set before migrating a legacy project.', { source: 'import' })
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
    { source: 'import' },
  )
}

async function onFilesSelected(paths: string[]) {
  const set = activeSet.value
  if (!set || !projectMeta.current) return
  showBrowser.value = false
  try {
    const res = await fetch('/api/images/register', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        projectUid: projectMeta.current.uid,
        setUid:     set.uid,
        filepaths:  paths,
        kind:       projectMeta.current.type,
      }),
    })
    const body = await res.json().catch(() => ({})) as { images?: any[]; error?: string }
    if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
    const imgs = body.images ?? []
    project.addImagesFromApi(set.uid, imgs)
    log.info(
      `Added ${imgs.length} image${imgs.length !== 1 ? 's' : ''} to "${set.name}".`,
      { source: 'import' }
    )
  } catch (e) {
    log.error(
      `Failed to register images: ${e instanceof Error ? e.message : String(e)}`,
      { source: 'import' }
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

  <ModuleLayout
    module="import"
    :allow-manage="true"
    :allow-delete="true"
    :show-filter="false"
    no-set-hint="Create a set to get started."
  >
    <template #actions="{ hasSet }">
      <button
        class="cc-btn cc-btn-primary"
        :disabled="!hasSet"
        @click="openFilePicker"
        v-tooltip.bottom="hasSet
          ? 'Browse and select microscopy image files to add to this set.'
          : 'Create or select a set first, then add images.'"
      >
        <i class="pi pi-plus" /> Add images
      </button>
      <button
        class="cc-btn"
        :disabled="!hasSet"
        @click="openMigrate"
        v-tooltip.bottom="hasSet
          ? 'Import images, segmentation and tracking from an old (R/Shiny) cecelia project.'
          : 'Create or select a set first.'"
      >
        <i class="pi pi-history" /> Migrate legacy project
      </button>
    </template>

    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="importDefs"
        :on-reload-defs="reloadDefs"
        module="import"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>
  </ModuleLayout>
</template>
