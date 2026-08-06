<!--
  Optical Flow — train and manage the flow-segmentation models.

  Scope is deliberately narrow: smoothing lives in Cleanup (it is general image cleanup) and the
  segmenter lives in Segment (users look for a segmenter there). Only TRAINING and the VAULT are new
  concerns, so only they live here. The old R version had a train-models page that hid what went into
  a model; the fix is that every parameter is a visible task param and the vault shows each model's
  manifest — not a prettier form.

  Ordinary module page: image table, task rail, nothing bespoke. Training is an ordinary task, so it
  gets progress, cancel, logs, QC and chainability for free.
-->
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import FlowModelVault from './opticalFlow/FlowModelVault.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: flowDefs, reload: reloadDefs } = useTaskDefs('opticalFlow')
</script>

<template>
  <ModuleLayout module="opticalFlow" :show-attrs="true" :show-filter="true"
    plots-label="Model vault"
    hint-key="opticalFlow" hint="Train on a smoothed movie; segment on the Segment page.">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="flowDefs"
        :on-reload-defs="reloadDefs"
        module="opticalFlow"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>
    <!-- The vault is not a plot; it is this page's second half. ModuleLayout's collapsible section
         is the module-page equivalent of the canvas panel shell, so it goes here. -->
    <template #plots="{ orderedUids }">
      <FlowModelVault :image-uids="orderedUids" />
    </template>
  </ModuleLayout>
</template>
