<script setup lang="ts">
import { computed } from 'vue'
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import SegmentationQcPanel from '../components/plots/SegmentationQcPanel.vue'
import { useTaskDefs } from '../composables/useTaskDefs'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useProjectStore } from '../stores/project'

const { defs: segmentDefs, reload: reloadDefs } = useTaskDefs('segment')
const projectMeta = useProjectMetaStore()
const project = useProjectStore()

// The segmentation to QC: the first selected image's active/first label set (they share a pipeline).
function qcValueName(uids: string[]): string {
  const uid = uids[0]
  if (!uid) return 'default'
  for (const s of project.sets) {
    const img = s.images.find(i => i.uid === uid)
    if (img) {
      const keys = Object.keys(img.labels ?? {})
      return img.activeValueName && keys.includes(img.activeValueName)
        ? img.activeValueName
        : (keys[0] ?? 'default')
    }
  }
  return 'default'
}
const projectUid = computed(() => projectMeta.current?.uid ?? '')
</script>

<template>
  <ModuleLayout module="segment" :show-attrs="true" :show-filter="true">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="segmentDefs"
        :on-reload-defs="reloadDefs"
        module="segment"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>

    <!-- Segmentation integrity (QC) plot below the image table -->
    <template #below-table="{ setUid, selectedUids }">
      <SegmentationQcPanel
        :project-uid="projectUid"
        :value-name="qcValueName(selectedUids)"
        :set-uid="setUid"
        :image-uids="selectedUids"
      />
    </template>
  </ModuleLayout>
</template>
