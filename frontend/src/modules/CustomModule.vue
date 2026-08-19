<!--
  Generic module page for a USER custom-module category that has no built-in page of its own
  (see docs/CUSTOM_MODULES.md). Routed as /custom/:category. Same layout as a built-in module page:
  pick images on the left, run the category's custom tasks in the TaskRunner on the right, inspect the
  results in the summary-plot canvas below. A custom task added to an EXISTING category (behaviour,
  segment, …) shows up on that category's real page instead; only brand-new categories land here.

  A plugin may ALSO name a built-in interactive plot to show here — `plugin.json` →
  `contributions.views`, by the stable id in `canvas/interactiveViews.ts`. That makes view IDS public,
  not components: renaming an id breaks installed plugins, rewriting the component behind it does not.
  See docs/todo/PLUGINS_PLAN.md → Decision 11.

  The canvas is fed by plot specs the module or PLUGIN ships in its own `plotDefinitions/` folder,
  declaring `module: "<category>"` — read by `Cecelia.user_plot_specs` and served through the same
  /api/plots/definitions route every built-in page uses. That is what lets a plugin provide a real
  module page (task form + plots) without shipping any Vue: an installed app has no Node/Vite to
  compile a component with. See docs/todo/PLUGINS_PLAN.md.
-->
<script setup lang="ts">
import { computed } from 'vue'
import { useRoute } from 'vue-router'
import ModuleLayout from '../components/ModuleLayout.vue'
import SummaryCanvas from '../components/canvas/SummaryCanvas.vue'
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
const cat = computed(() => customModules.categories.find(c => c.name === category.value))
const cohortFuns = computed(() => cat.value?.cohortFuns ?? [])
// Interactive plots this plugin asked for (PLUGINS_PLAN Decision 11) — passed straight through to the
// canvas, which owns the registry and so is the half that can tell whether an id resolves.
const views = computed(() => cat.value?.views ?? [])
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
    <template #plots="{ selectedUids }">
      <SummaryCanvas :image-uids="selectedUids" :module="category" :views="views" />
    </template>
  </ModuleLayout>
</template>
