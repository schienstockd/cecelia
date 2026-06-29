<script setup lang="ts">
import { ref, computed } from 'vue'
import { useProjectStore } from '../stores/project'
import { useLogStore } from '../stores/log'
import { useProjectMetaStore } from '../stores/projectMeta'
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import FileBrowser from '../components/FileBrowser.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: importDefs, reload: reloadDefs } = useTaskDefs('importImages')
const project     = useProjectStore()
const log         = useLogStore()
const projectMeta = useProjectMetaStore()

const activeSet   = computed(() => project.activeSet())
const showBrowser = ref(false)

function openFilePicker() {
  if (!activeSet.value) {
    log.warn('Create or select a set before adding images.', { source: 'import' })
    return
  }
  showBrowser.value = true
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
