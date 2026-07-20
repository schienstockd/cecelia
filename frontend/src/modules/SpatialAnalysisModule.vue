<!--
  Spatial analysis module page. Pick image(s), then run the spatial-analysis tasks in the right-hand
  TaskRunner (all IMAGE-scope):
   • Neighbour graph (squidpy) — the shared substrate.
   • Contact statistics — pairwise cell-type contact log-odds (association / avoidance).
   • Aggregate detection — points (DBSCAN) and mesh (surface proximity) routes.
   • Cell contacts — points (kNN) and mesh (surface distance) routes.

  A SEPARATE page from Region clustering (Decision 12). Spatial readouts (log-odds tables, region
  membership) are exposed via MCP (get_spatial_stats) and land as obs columns / populations; dedicated
  spatial-result plots on the module page are a later addition, so this page has no #plots canvas yet.
  Population selectors accept any cell poptype (flow / live / clust / region), so cross-poptype spatial
  questions work here (docs/todo/SPATIAL_REGIONS_PLAN.md).
-->
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: spatialDefs, reload: reloadDefs } = useTaskDefs('spatialAnalysis')
</script>

<template>
  <ModuleLayout module="spatialAnalysis" :show-attrs="true" :show-filter="true">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="spatialDefs"
        :on-reload-defs="reloadDefs"
        module="spatialAnalysis"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>
  </ModuleLayout>
</template>
