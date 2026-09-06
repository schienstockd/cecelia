<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import CorrectionPlanPanel from '../components/CorrectionPlanPanel.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: cleanupDefs, reload: reloadDefs } = useTaskDefs('cleanupImages')
</script>

<template>
  <ModuleLayout module="cleanup" :show-attrs="true" :show-filter="true">
    <template #right="{ selectedUids, selectedNames }">
      <div class="cleanup-right">
        <CorrectionPlanPanel :selected-uids="selectedUids" />
        <TaskRunner
          :defs="cleanupDefs"
          :on-reload-defs="reloadDefs"
          module="cleanup"
          :selected-uids="selectedUids"
          :selected-names="selectedNames"
        />
      </div>
    </template>
  </ModuleLayout>
</template>

<style scoped>
.cleanup-right {
  display: flex;
  flex-direction: column;
  gap: 8px;
  height: 100%;
  min-height: 0;
}
</style>
