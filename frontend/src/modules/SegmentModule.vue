<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
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
  </ModuleLayout>
</template>
