<!--
  Behaviour analysis module page. Pick one OR MORE images (multi-select), then:
   • run behaviour tasks (HMM states/transitions, the hmm composite) in the right-hand TaskRunner —
     these are SET-SCOPE (fitted jointly across all selected images);
   • inspect results in the summary-plot canvas below the table (HMM state frequencies, track speed,
     …). Per-module canvas: only `behaviourAnalysis` plot specs are offered.
-->
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import CollapsibleSection from '../components/CollapsibleSection.vue'
import SummaryCanvas from '../components/canvas/SummaryCanvas.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: behaviourDefs, reload: reloadDefs } = useTaskDefs('behaviour')
</script>

<template>
  <ModuleLayout module="behaviourAnalysis" :show-attrs="true" :show-filter="true">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="behaviourDefs"
        :on-reload-defs="reloadDefs"
        module="behaviour"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>
    <template #below-table="{ selectedUids }">
      <CollapsibleSection label="Plots" :max-height="'none'">
        <SummaryCanvas :image-uids="selectedUids" module="behaviourAnalysis" />
      </CollapsibleSection>
    </template>
  </ModuleLayout>
</template>
