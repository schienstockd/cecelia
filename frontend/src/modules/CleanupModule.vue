<script setup lang="ts">
import { ref } from 'vue'
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import CorrectionPlanPanel from '../components/CorrectionPlanPanel.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: cleanupDefs, reload: reloadDefs } = useTaskDefs('cleanupImages')

// The correction-plan surface is a FLOATING panel now (open/close persists via its FloatingPanel
// storage-key so its position + size survive a module switch). One boolean here + a launcher in the
// TaskRunner's PaneExpandBar slot — parallel to the Viewer / Lab log launchers on the app header,
// but scoped to the cleanup module because that's the only place the plan applies.
const PLAN_OPEN_KEY = 'cc.correction-plan.open'
const planOpen = ref<boolean>(false)
try { planOpen.value = localStorage.getItem(PLAN_OPEN_KEY) === '1' } catch { /* first-run */ }
function togglePlan(): void {
  planOpen.value = !planOpen.value
  try { localStorage.setItem(PLAN_OPEN_KEY, planOpen.value ? '1' : '0') } catch { /* ignore */ }
}
function closePlan(): void {
  planOpen.value = false
  try { localStorage.setItem(PLAN_OPEN_KEY, '0') } catch { /* ignore */ }
}
</script>

<template>
  <ModuleLayout module="cleanup" :show-attrs="true" :show-filter="true">
    <template #right="{ selectedUids, selectedNames }">
      <div class="cleanup-right">
        <TaskRunner
          :defs="cleanupDefs"
          :on-reload-defs="reloadDefs"
          module="cleanup"
          :selected-uids="selectedUids"
          :selected-names="selectedNames"
        >
          <template #bar-actions>
            <button
              class="pane-btn cc-btn cc-btn-bare cc-btn-icon"
              :class="{ 'cc-btn-on': planOpen }"
              :aria-pressed="planOpen"
              @click="togglePlan"
              v-tooltip.left="planOpen ? 'Hide the correction plan' : 'Show the correction plan'">
              <i class="pi pi-list-check" />
            </button>
          </template>
        </TaskRunner>
        <CorrectionPlanPanel v-if="planOpen" :selected-uids="selectedUids" @close="closePlan" />
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
.pane-btn { font-size: var(--cc-fs-xs); }
</style>
