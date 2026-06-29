<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: cleanupDefs, reload: reloadDefs } = useTaskDefs('cleanupImages')
</script>

<template>
  <ModuleLayout module="cleanup" :show-attrs="true" :show-filter="true">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="cleanupDefs"
        :on-reload-defs="reloadDefs"
        module="cleanup"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>
  </ModuleLayout>
</template>
