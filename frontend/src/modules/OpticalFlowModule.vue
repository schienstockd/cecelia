<!--
  Optical Flow — train and manage the flow-segmentation models.

  Scope is deliberately narrow: smoothing lives in Cleanup (it is general image cleanup) and the
  segmenter lives in Segment (users look for a segmenter there). Only TRAINING and the VAULT are new
  concerns, so only they live here. The old R version had a train-models page that hid what went into
  a model; the fix is that every parameter is a visible task param and the vault shows each model's
  manifest — not a prettier form.

  Ordinary module page: image table, task rail, a `#plots` canvas, nothing bespoke. Training is an
  ordinary task, so it gets progress, cancel, logs, QC and chainability for free — and the canvas
  (FlowPlots) is the same shell every other page's plots use, so a trained model can be inspected
  here rather than only on the Analysis board.

  The vault is a FLOATING panel (the app's `FloatingPanel`, same component as Viewer and Lab log),
  toggled from the page actions and remembered across visits. It was an inline list in the page's
  collapsible section first and read as filler — a manager you open when you want it is the right
  shape, and reusing the app component means drag/resize/collapse/persistence come for free.
-->
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import FloatingPanel from '../components/FloatingPanel.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import FlowModelVault from './opticalFlow/FlowModelVault.vue'
import FlowPlots from './opticalFlow/FlowPlots.vue'
import { useTaskDefs } from '../composables/useTaskDefs'
import { useSettingsStore } from '../stores/settings'

const { defs: flowDefs, reload: reloadDefs } = useTaskDefs('opticalFlow')
const settings = useSettingsStore()
</script>

<template>
  <ModuleLayout module="opticalFlow" :show-attrs="true" :show-filter="true" plots-label="Flow model"
    hint-key="opticalFlow" hint="Train on a smoothed movie; segment on the Segment page.">
    <template #actions>
      <button class="cc-btn cc-btn-ghost" :class="{ 'cc-btn-on': settings.flowVaultOpen }"
              v-tooltip.top="'Show or hide the trained model vault'"
              @click="settings.flowVaultOpen = !settings.flowVaultOpen">
        <i class="pi pi-database" /> Model vault
      </button>
    </template>

    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="flowDefs"
        :on-reload-defs="reloadDefs"
        module="opticalFlow"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>

    <template #plots="{ selectedUids }">
      <FlowPlots :image-uids="selectedUids" />
    </template>
  </ModuleLayout>

  <FloatingPanel v-if="settings.flowVaultOpen" title="Model vault" icon="pi-database"
                 storage-key="flowVault" :default-w="340" :default-h="360"
                 @close="settings.flowVaultOpen = false">
    <FlowModelVault />
  </FloatingPanel>
</template>
