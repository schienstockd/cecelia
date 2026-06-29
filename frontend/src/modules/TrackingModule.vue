<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import CollapsibleSection from '../components/CollapsibleSection.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import GatingPlots from './gate/GatingPlots.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: trackingDefs, reload: reloadDefs } = useTaskDefs('tracking')
</script>

<template>
  <ModuleLayout module="tracking" :show-attrs="true" :show-filter="true">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="trackingDefs"
        :on-reload-defs="reloadDefs"
        module="tracking"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>
    <!-- Track-property gating: the SAME gating canvas as the Gate page, in track mode (one point
         per track; gate on motility / per-track aggregates). Active when exactly one image is
         selected. Reuses GatingPlots via popType — no track-specific component. -->
    <template #below-table="{ selectedUids }">
      <CollapsibleSection label="Track gating" :max-height="'none'">
        <GatingPlots :image-uid="selectedUids.length === 1 ? selectedUids[0] : null" pop-type="track" />
      </CollapsibleSection>
    </template>
  </ModuleLayout>
</template>
