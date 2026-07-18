<!--
  Generic module page for a USER custom-module category that has no built-in page of its own
  (see docs/CUSTOM_MODULES.md). Routed as /custom/:category. Same layout as a built-in module page —
  pick images on the left, run the category's custom tasks in the TaskRunner on the right — but with
  no plot canvas (a custom category has no registered plot specs). A custom task added to an EXISTING
  category (behaviour, segment, …) shows up on that category's real page instead; only brand-new
  categories land here.
-->
<script setup lang="ts">
import { computed } from 'vue'
import { useRoute } from 'vue-router'
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import { useTaskDefs } from '../composables/useTaskDefs'
import { useCustomModulesStore } from '../stores/customModules'

const route    = useRoute()
// Category comes from the route; useTaskDefs is re-created per navigation because vue-router reuses
// the component instance across /custom/:category changes only when the key differs — main.ts keys
// the route by full path, so a fresh instance (and fresh defs) is created per category.
const category = computed(() => String(route.params.category ?? ''))
const { defs, reload } = useTaskDefs(category.value)
// cohort funs come from the backend (funNames ∩ COHORT_METRICS), so the "Check cohort" button appears
// automatically for a custom module that registered cohort metrics — no hardcoded per-page list.
const customModules = useCustomModulesStore()
const cohortFuns = computed(() =>
  customModules.categories.find(c => c.name === category.value)?.cohortFuns ?? [])
</script>

<template>
  <ModuleLayout :module="category" :show-attrs="true" :show-filter="true" :cohort-funs="cohortFuns">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="defs"
        :on-reload-defs="reload"
        :module="category"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>
  </ModuleLayout>
</template>
