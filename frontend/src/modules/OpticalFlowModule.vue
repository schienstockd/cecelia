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

  The vault lives ON that canvas (a `CanvasSidePanel`, toggled from the canvas bar like the
  population manager), not in a top-level `FloatingPanel` — a canvas-scoped manager in the app's
  window layer just competes with the Viewer and the Lab log for the same corner.
-->
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import FlowPlots from './opticalFlow/FlowPlots.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: flowDefs, reload: reloadDefs } = useTaskDefs('opticalFlow')
</script>

<template>
  <ModuleLayout module="opticalFlow" :show-attrs="true" :show-filter="true" plots-label="Flow model"
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

    <template #plots="{ selectedUids }">
      <FlowPlots :image-uids="selectedUids" />
    </template>
  </ModuleLayout>
</template>
