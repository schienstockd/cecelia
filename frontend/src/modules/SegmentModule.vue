<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import SummaryCanvas from '../components/canvas/SummaryCanvas.vue'
import { useTaskDefs } from '../composables/useTaskDefs'
import { useSettingsStore } from '../stores/settings'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { correctionLaunch } from '../utils/correctionSelections'
import { openViewerWindow } from '../utils/viewerWindow'

const { defs: segmentDefs, reload: reloadDefs } = useTaskDefs('segment')
const settings = useSettingsStore()
const project = useProjectStore()
const projectMeta = useProjectMetaStore()

// The correction cockpit, reachable from the page whose results it fixes
function openCorrection(selectedUids: string[]) {
  const go = correctionLaunch('segment', selectedUids, project.openImageUid)
  settings.correctionCockpitMode = go.mode
  settings.correctionCockpitOpen = true
  if (go.openInViewer) openViewerWindow({ projectUid: projectMeta.current?.uid ?? '', imageUid: go.openInViewer })
}
</script>

<template>
  <ModuleLayout module="segment" :show-attrs="true" :show-filter="true">
    <template #actions="{ selectedUids }">
      <button class="cc-btn cc-btn-ghost cc-btn-dense" data-guide="segment.correct"
              v-tooltip.bottom="'Fix labels by hand: merge, split or remove cells in the viewer'" @click="openCorrection(selectedUids)">
        <i class="pi pi-wrench" /> Correct labels
      </button>
    </template>
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="segmentDefs"
        :on-reload-defs="reloadDefs"
        module="segment"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>

    <!-- Segmentation integrity (QC) plots: the canonical summary canvas, filtered to `segment` plot
         specs (segmentation_qc.json — cell count / morphology over the `labels` popType). One
         selectable population per segmentation value_name, so B/T plot side by side. -->
    <template #plots="{ selectedUids }">
      <SummaryCanvas :image-uids="selectedUids" module="segment" />
    </template>
  </ModuleLayout>
</template>
