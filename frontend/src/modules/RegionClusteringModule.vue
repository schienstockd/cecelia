<!--
  Region clustering module page. Pick one OR MORE images, then:
   • run region clustering (clustRegions.cluster) in the right-hand TaskRunner — SET-SCOPE (composition
     vectors clustered over the pooled set so region IDs are comparable across images);
   • below/beside the table: explore the regions (composition heatmap + UMAP) and define populations by
     ticking region IDs into them — the `region` pop type (a cell carries BOTH a cluster and a region
     label, docs/todo/SPATIAL_REGIONS_PLAN.md).

  Spatial regions are neighbourhood-composition niches ("what cell types surround each cell"). This is a
  SEPARATE page from Spatial analysis (Decision 12). Reuses the cluster canvas (ClusterPlots) with
  pop-type="region" — no bespoke region panel (feedback: use the existing framework).

  Like its two sibling clustering pages, this page only DEFINES populations. The `region` population
  summary lives on **Spatial**, next to the other spatial readouts.
-->
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner from '../tasks/TaskRunner.vue'
import ClusterPlots from './cluster/ClusterPlots.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs: regionDefs, reload: reloadDefs } = useTaskDefs('clustRegions')
</script>

<template>
  <ModuleLayout module="clustRegions" :show-attrs="true" :show-filter="true" plots-label="Regions">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="regionDefs"
        :on-reload-defs="reloadDefs"
        module="clustRegions"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>
    <template #plots="{ selectedUids, selectUids }">
      <ClusterPlots :image-uids="selectedUids" :select-uids="selectUids" pop-type="region" />
    </template>
  </ModuleLayout>
</template>
