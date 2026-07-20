<!--
  Spatial analysis module page. Pick image(s), then run the spatial-analysis tasks in the right-hand
  TaskRunner (all IMAGE-scope):
   • Neighbour graph (squidpy) — the shared substrate.
   • Contact statistics — pairwise cell-type contact log-odds (association / avoidance).
   • Aggregate detection — points (DBSCAN) and mesh (surface proximity) routes.
   • Cell contacts — points (kNN) and mesh (surface distance) routes.

  A SEPARATE page from Region clustering (Decision 12). Spatial readouts land as obs columns /
  populations (composition `spatial.comp.*`, `*.cell.is.aggregate`, `regions.*`) and are exposed via MCP
  (get_spatial_stats). The #plots canvas is the generic SummaryCanvas (registry-driven, docs/PLOTS.md) —
  it plots those per-cell spatial measures like any other; new spatial plots slot in as registry specs
  over time (Decision 16). Population selectors accept any cell poptype (flow / live / clust / region),
  so cross-poptype spatial questions work here (docs/todo/SPATIAL_REGIONS_PLAN.md).
-->
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import SummaryCanvas from '../components/canvas/SummaryCanvas.vue'
import CollapsibleSection from '../components/CollapsibleSection.vue'
import SpatialContactHeatmap from './spatial/SpatialContactHeatmap.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: spatialDefs, reload: reloadDefs } = useTaskDefs('spatialAnalysis')
</script>

<template>
  <ModuleLayout module="spatialAnalysis" :show-attrs="true" :show-filter="true" plots-label="Spatial">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="spatialDefs"
        :on-reload-defs="reloadDefs"
        module="spatialAnalysis"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>
    <template #plots="{ selectedUids }">
      <SummaryCanvas :image-uids="selectedUids" module="spatialAnalysis" />
    </template>
    <template #below-table="{ selectedUids }">
      <CollapsibleSection label="Cell-type contacts (log-odds)" max-height="none"
        :storage-key="'cc-contact-open:spatialAnalysis'">
        <SpatialContactHeatmap :image-uids="selectedUids" />
      </CollapsibleSection>
    </template>
  </ModuleLayout>
</template>
