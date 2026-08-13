<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import SummaryCanvas from '../components/canvas/SummaryCanvas.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: segmentDefs, reload: reloadDefs } = useTaskDefs('segment')
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

    <!-- Segmentation integrity (QC) plots: the canonical summary canvas, filtered to `segment` plot
         specs (segmentation_qc.json — cell count / morphology over the `labels` popType). One
         selectable population per segmentation value_name, so B/T plot side by side. -->
    <template #plots="{ selectedUids }">
      <SummaryCanvas :image-uids="selectedUids" module="segment" />
    </template>
  </ModuleLayout>
</template>
