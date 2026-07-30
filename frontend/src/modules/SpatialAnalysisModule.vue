<!--
  Spatial analysis module page. Pick image(s), then run the spatial-analysis tasks in the right-hand
  TaskRunner (all IMAGE-scope). Order matters — the graph comes first:
   • Neighbour graph (squidpy) — THE substrate, persisted to spatialGraph/{suffix}.h5ad. Pop-agnostic:
     it stores node identity only, so one graph serves every later population question. Every other
     readout here (and region clustering on its own page) LOADS it rather than building its own
     (Decision 17).
   • Interaction matrix — pairwise log-odds (association / avoidance) PLUS a permutation test, so you can
     tell a real pattern from a random arrangement of the same cell types (Decision 18). Needs no
     regions: interactions are just a labelling of the graph.
   • Aggregate detection — points (DBSCAN) and mesh (surface proximity) routes.
   • Cell contacts — PER-CELL columns (contact flag / µm to the nearest target / contact id); points
     (kNN) and mesh (surface distance) routes. NOT the same thing as the Interaction matrix above: that
     one is a population×population summary in spatialStats/, this one annotates individual cells.

  A SEPARATE page from Region clustering (Decision 12). Spatial readouts land as obs columns /
  populations (composition `spatial.comp.*`, `*.cell.is.aggregate`, `regions.*`) and are exposed via MCP
  (get_spatial_stats). The #plots canvas is the generic SummaryCanvas (registry-driven, docs/PLOTS.md).
  Population selectors accept any cell poptype (flow / live / clust / region), so cross-poptype spatial
  questions work here (docs/todo/SPATIAL_REGIONS_PLAN.md).
-->
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import SummaryCanvas from '../components/canvas/SummaryCanvas.vue'
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
  </ModuleLayout>
</template>
