<!--
  Model training — train and manage every kind of model this app knows about (optical-flow
  segmentation, self-supervised denoise, and whatever comes next).

  Scope is deliberately narrow: smoothing lives in Cleanup, the segmenter lives in Segment, the
  denoise INFERENCE step lives in Cleanup. Only TRAINING and the VAULT are new concerns, so only
  they live here. The old R version had a train-models page that hid what went into a model; the
  fix is that every parameter is a visible task param and the vault shows each model's manifest —
  not a prettier form.

  One module page hosts multiple training scenarios (opticalFlow.train + opticalFlow.trainSupportDenoise
  today; more later). The vault manager on the canvas has a kind chip row that switches which vault
  is being browsed — one manager, one refresh path, one delete/rename path.

  Ordinary module page: image table, task rail, a `#plots` canvas, nothing bespoke. Training is an
  ordinary task, so it gets progress, cancel, logs, QC and chainability for free.

  The vault lives ON that canvas (a `CanvasSidePanel`, toggled from the canvas bar like the
  population manager), not in a top-level `FloatingPanel` — a canvas-scoped manager in the app's
  window layer just competes with the Viewer and the Lab log for the same corner.

  The task namespace stays `opticalFlow.*` and the module key stays `opticalFlow` — those are
  stored in ccid.json chain state, so renaming them would break every persisted chain. The page's
  display name (route, sidebar, task category) is the honest one.
-->
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import ModelPlots from './modelTraining/ModelPlots.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: flowDefs, reload: reloadDefs } = useTaskDefs('opticalFlow')
</script>

<template>
  <ModuleLayout module="opticalFlow" :show-attrs="true" :show-filter="true" plots-label="Training">
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
      <ModelPlots :image-uids="selectedUids" />
    </template>
  </ModuleLayout>
</template>
