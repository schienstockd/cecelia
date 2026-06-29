<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: testDefs, reload: reloadDefs } = useTaskDefs('testTasks')
</script>

<template>
  <ModuleLayout module="test" :show-filter="false">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="testDefs"
        :on-reload-defs="reloadDefs"
        module="test"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>
  </ModuleLayout>
</template>
