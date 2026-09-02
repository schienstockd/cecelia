<script setup lang="ts">
// Preprocessing — image-in → new-image-out ops (crop today; MIP / bin / resample land here).
// Backend category `editImages/` is the source of task defs; `copyImage` is `hidden:true` because
// Manage images owns that dialog. See docs/MODULES.md → *Module page*.
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: preprocessDefs, reload: reloadDefs } = useTaskDefs('editImages')
</script>

<template>
  <ModuleLayout module="preprocess" :show-attrs="true" :show-filter="true">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="preprocessDefs"
        :on-reload-defs="reloadDefs"
        module="preprocess"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>
  </ModuleLayout>
</template>
